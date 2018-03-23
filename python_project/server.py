import asyncio
import config 
import sys
import time
import ssl
import logging 
import datetime
import json
import aiohttp
import async_timeout
# send response to client 
def send_resp(transport, message):
	transport.write(message.encode())
	logger.info('output response to Client: {!r}'.format(message))

#close connection 
def close_connection(transport):
	transport.close()
	peername = transport.get_extra_info('peername')
	logger.info('Connection lost {}\n'.format(peername))

#handling exception from the servers
def server_exceptions(loop, context):
	try:
		exception = context('exception')
		if isinstance(exception, ConnectionRefusedError):
			logger.error('Cannot connect to neighboring server')
		else:
			logger.error('ERROR: {}'.format(context['exception']))
	except KeyError:
		logger.error('Interrupted by the keyboard: {}'.format(context['message']))

# Class for creating server 
class ProxyHerdServerClientProtocol(asyncio.Protocol):
	client_time_stamps = {}

	def __init__(self, server_name):
		self.name =  server_name
		self.floodlist = config.servers_floodlist[server_name]


	def  connection_made(self, transport):
		peername = transport.get_extra_info('peername')
		logger.info('A connection from {}'.format(peername))
		self.transport = transport

	def data_received(self, data):
		message = data.decode()
		logger.info('Data received: {!r}'.format(message))
		
		input_msg = message.split()
		command = input_msg[0]
		arguments = input_msg[1:]         
	
		if(command == 'IAMAT' and self.parse_IAMAT(arguments)):
			logger.info('Ready to respond the IAMAT input')
			respond_msg =  self.reply_IAMAT(input_msg[1],input_msg[2],input_msg[3]) + '\n'
		elif (command == 'WHATSAT' and self.parse_WHATSAT(arguments)):
			self.sendTo_WHATSAT(input_msg[1],input_msg[2],input_msg[3],message)
			return
		elif (command == 'AT'):
			respond_msg = self.respond_AT(input_msg[3], message)
		else:
			respond_msg = '? ' +  message + '\n' 

		send_resp(self.transport, respond_msg)
#		close_connection(self.transport)

	def get_client_loc(self,client_id):
		location_str =  ProxyHerdServerClientProtocol.client_time_stamps[client_id].split()[4]
		return self.extract_location(location_str)
	
	def extract_location (self, location_str):
		lat, lng = '',''
		sp1, sp2 = True,True
		for ch in location_str:
			if((not sp2) and (ch == '+' or ch == '-')):
				sp1 = False
			if sp1:
				lat += ch
			else:
				lng += ch
			if sp2: sp2 = False
		return lat, lng



	def parse_tm_stamp(self, stamp):
		return float(stamp.split()[5])


	def update_client_time_stamp(self, client_id, stamp):
		if stamp.split()[3] !=  client_id:
			logger.error ('invalid call')
			return False
		isUpdate = False
		try:
		    if self.parse_tm_stamp(stamp) > self.parse_tm_stamp(ProxyHerdServerClientProtocol.client_time_stamps[client_id]):
		        ProxyHerdServerClientProtocol.client_time_stamps[client_id] = stamp
		        isUpdate = True
		except KeyError:
			ProxyHerdServerClientProtocol.client_time_stamps[client_id]=stamp
			isUpdate = True
		if isUpdate:
			logger.info('Location has updated for {}'.format(client_id))
		else:
			logger.info('Updated location failed for {}'.format(client_id))
		return isUpdate

	def floodAlgorithm(self, msg):
       		countlst = msg.split()[5:]
        	for server_name in self.floodlist:
            	    if server_name not in countlst:
                        self.propagateTo(server_name, config.servers_port[server_name], msg)
	
	def propagateTo(self, name, port, msg):
             coro = loop.create_connection(lambda: ClientProtocol(msg, name), config.servers_host, port)
             loop.create_task(coro)
	
	def verify_location(self, location_str, client_id):
		try:
		    lat, lng = self.extract_location(location_str)
		    lat_val, lng_val = float(lat), float(lng)
		except ValueError:
		    return False
		if lat_val > 90 or lat_val < -90:  return False
		if lng_val > 180  or lng_val <-180: return False
		return True 
	
	def verify_time(self, tm_str):
		try:
		     tm = float(tm_str)
		     datetime.datetime.utcfromtimestamp(tm)
		except ValueError:
		     return False
		return True

	def parse_IAMAT(self, arguments):
		if(len(arguments) != 3): 
			logger.error('Invalid number of arguments in IAMAT')
			return False
		if(not self.verify_location(arguments[1], arguments[0])):
			logger.error('Location does not use ISO 6709')
			return False
		if(not self.verify_time(arguments[2])):
			logger.error('Time did not follow POSIX time format')
			return False
		return True

	def parse_WHATSAT(self, arguments):
            if (len(arguments) != 3):
                logger.error('Invalid number of arguments for WHATSAT')
                return False
        # Check if client_id location exists
            
            try:
                ProxyHerdServerClientProtocol.client_time_stamps[arguments[0]]
            except KeyError:
                logger.error('Client does not  have a location yet')
                return False
        # Check type
            try:
                r = float(arguments[1])
                bd = int(arguments[2])
            except ValueError:
                logger.error('Wrong type for radius or bound')
                return False
        # Check range
            if r > 50 or r < 0:
                logger.error('radius is out of range')
                return False
            if bd > 20 or bd < 0:
                logger.error('bound is out of bound')
                return False
            return True

	def respond_AT(self, client_id, msg):
       
                primary_server = msg.split()[-1]
                peername = self.transport.get_extra_info('peername')
                logger.info('{} connection server {} propagating data'.format(peername, primary_server))

		# Update client's AT stamp
                stamp = ' '.join(msg.split()[:6])
                self.update_client_time_stamp(client_id, stamp)
         
                fd_msg = msg + ' {}'.format(self.name)
                self.floodAlgorithm(fd_msg)
                return '{} received updated location'.format(self.name)

	
	def reply_IAMAT(self, client_id, loc, tm):
		# cal time difference
		tm_diff =  time.time() - float(tm)
		tm_diff_str = '{:.9f}'.format(tm_diff)
		if tm_diff > 0:
			tm_diff_str = '+' + tm_diff_str
		
		# udpate the time with the new time stamp in AT
		tm_stamp = 'AT {} {} {} {} {}'.format(self.name, tm_diff_str, client_id,loc,tm)
		if self.update_client_time_stamp(client_id, tm_stamp):
			# propagate AT tm_stamp to other servers
			self.floodAlgorithm(tm_stamp + ' ' + self.name)
		else:
			logger.info('Failed to propagate location')
		
		return ProxyHerdServerClientProtocol.client_time_stamps[client_id]

	def sendTo_WHATSAT(self, client_id, rd_km, info_bd, msg):
		# get client location with the client id 
		usr_loc = self.get_client_loc(client_id)
		# get the radius 
		rd_m = str(float(rd_km) * 1000)
		#build http request to the Google place API 	
		location_str = '{},{}'.format(usr_loc[0], usr_loc[1])
		tg = 'location={}&radius={}&key={}'.format(location_str,
rd_m, config.key)
		base_url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?' + tg
		loop = asyncio.get_event_loop()
		display_client_msg = ProxyHerdServerClientProtocol.client_time_stamps[client_id]
		logger.info('========Sending HTTP request to Google Server====')
		asyncio.ensure_future(self.fetch(display_client_msg,base_url))
		
	@asyncio.coroutine
	async def fetch(self,ATmsg,url):
		logger.info('========Fetching nearby places=========')
		async with aiohttp.ClientSession() as session:
			with async_timeout.timeout(10):
				async with session.get(url) as response: 
					json = await response.text()
					send_resp(self.transport, ATmsg)
					send_resp(self.transport, '\n')		
					send_resp(self.transport, json)

class ClientProtocol(asyncio.Protocol):
    def __init__(self, message, name):
        self.message = message
        self.client_name = name

    def connection_made(self, transport):
        logger.info('Connected to server {}'.format(self.client_name))
        self.transport = transport
        self.transport.write(self.message.encode())
        logger.info('Data has propogated to server {}\n'.format(self.client_name))

    def connection_lost(self, exc):
        self.transport.close()
        logger.info('Dropped connection to server {}\n'.format(self.client_name))

if __name__ == '__main__':
	# check for number of arguments 
	if (len(sys.argv) != 2):
		print('Invaid server name.')
		exit(1)
	# check for invalid server name 
	server_name = sys.argv[1]
	if not server_name in  config.servers:
		print ('The server name does not exsit. Please use Goloman,Hands,Holiday,Welsh or Wilkes')
		exit(1)	    
	port_num = config.servers_port[server_name]
	
	# set up logging 
	logger =  logging.getLogger(server_name) 
	
	log_msg_format = '%(asctime)s - %(levelname)s (%(name)s) : %(message)s'
	log_formatter = logging.Formatter(log_msg_format)

	log_destination ='./logs/' + server_name.lower() + '.log'
	file_handler = logging.FileHandler(log_destination, mode= 'w')
	file_handler.setFormatter(log_formatter)
	
	stream_handler = logging.StreamHandler()
	stream_handler.setFormatter(log_formatter)
	
	logger.setLevel('INFO')
	logger.addHandler(file_handler)
	logger.addHandler(stream_handler)

	loop = asyncio.get_event_loop()
	loop.set_exception_handler(server_exceptions)
	coro = loop.create_server(lambda: ProxyHerdServerClientProtocol(server_name), '127.0.0.1',port_num)
	server = loop.run_until_complete(coro)

	logger.info('Serving on {}'.format(server.sockets[0].getsockname()))
	try:
		loop.run_forever()
	except KeyboardInterrupt:
		pass

	server.close()
	loop.run_until_complete(server.wait_closed())
	loop.close()
