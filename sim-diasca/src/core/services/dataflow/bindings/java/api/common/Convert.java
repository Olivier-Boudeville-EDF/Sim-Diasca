/**
 * 
 */
package common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.ericsson.otp.erlang.*;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 * 
 * Convert MAp Java to OtpErlangList
 */
public final class Convert {
	public static OtpErlangList MaptoOtpErlangList(Map<Object, Object> map) {
		
		List<OtpErlangTuple> listOtp = new ArrayList<OtpErlangTuple>();
		
		Set<Entry<Object, Object>> setMap = map.entrySet();
		Iterator<Entry<Object, Object>> it = setMap.iterator();
		while(it.hasNext()) {
			Entry<Object, Object> i = it.next();
			OtpErlangObject[] elems = new OtpErlangObject[2];
            elems[0] = new OtpErlangAtom(i.getKey().toString());
            elems[1] = new OtpErlangBinary(i.getValue().toString().getBytes());
            OtpErlangTuple t = new OtpErlangTuple(elems);
            listOtp.add(t);
		}
		return new OtpErlangList(listOtp.toArray(new OtpErlangObject[0]));
	}
}
