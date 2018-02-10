
import java.util.concurrent.atomic.AtomicIntegerArray;

public class GetNSet implements State {

    private AtomicIntegerArray value;
    private byte maxval;
    private void insertAintArr (byte [] v) {
        int length = v.length;
        value = new AtomicIntegerArray(length);
        for (int i = 0 ; i < length; i++) {
            value.set(i, v[i]);
        }
    }

    GetNSet(byte[] v) {
        insertAintArr(v);
        maxval = 127;
    }

    GetNSet(byte[] v, byte m) {
        insertAintArr(v);
        maxval = m;
    }

    public int size() {
        return value.length();
    }

    public byte[] current() {
        byte [] ans =  new byte[size()];
        for (int j = 0; j < value.length(); j++) {
            ans[j] = (byte)value.get(j);
        }
        return ans;
    }
    public boolean swap(int i, int j) {
        int iValue = value.get(i);
        int jValue =  value.get(j);
        if (iValue <=0 || jValue > maxval) return false;
        value.set(i, iValue-1);
        value.set(j, jValue+1);
        return true;
    }






}
