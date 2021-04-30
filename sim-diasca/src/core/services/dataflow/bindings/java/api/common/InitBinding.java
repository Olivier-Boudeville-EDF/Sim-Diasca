/**
 * Initialisation of the Java binding on a computing host.
 *
 * @author Julien Guillemineau (julien-externe.guillemineau@edf.fr)
 *
 */
package common;

import com.ericsson.otp.erlang.*;


/**
 * Entry point of the Java binding to the native (Erlang) part of the engine.
 *
 */
public class InitBinding {

	public static void main(String[] args) throws Exception {

		/* The host name is appended automatically to the identifier, and the
		 * port number is chosen by the underlying system:
		 *
		 */
		OtpNode node = new OtpNode("Sim-Diasca-java-binding-node");

		/* Registered name besides the PID, so that the Erlang part can find the
		 * corresponding pseudo-process:
		 *
		 */
		OtpMbox mbox = node.createMbox("Sim-Diasca-java-entry-point");

		OtpErlangObject[] elements =
			((OtpErlangTuple) mbox.receive()).elements();

		OtpErlangPid from = (OtpErlangPid)elements[0];

		try {
			new Thread(mbox.getName()) {

				public void run() {

					boolean run = true;

					while (run) {
						try {
							run = processRun(from, mbox.receive(), mbox);
						} catch (Exception e) {
							mbox.send(from, getAnswer(e));
						}
					}

					if(!run) {

						String error = new OtpErlangString(
						  "The incoming message is not a tuple")
						mbox.send("NoPid",new OtpErlangTuple(error));
					}
				}
			}.start();

		} catch (Exception e) {

			mbox.send(from, getAnswer(e));

		} finally {
			mbox.close();
		}
	}

	/**
	 * This function initializes the global state of the interpreter, stored in
	 * the InterpreterState class.
	 *
	 */
	protected static boolean processRun(OtpErlangPid from, OtpErlangObject msg,
	  OtpMbox mbox)
		throws OtpErlangDecodeException, OtpErlangException, Exception {

		if(msg instanceof OtpErlangTuple) {
			OtpErlangObject[] elements = ((OtpErlangTuple) msg).elements();

			/* Sets the global_communicator responsible for communicating with
			 * the Erlang core of the engine:
			 *
			 */
			InterpreterState.global_communicator = new ErlangCommunicator(from,
			  mbox);

			/* Binds the function defined below, handling messages from the
			 * Erlang part of the engine, to ErlPort's own message handler.
			 *
			 */
			InterpreterState.global_message_handler = new MessageHandler(
			  InterpreterState.global_communicator);

			InterpreterState.global_message_handler.handle_message(elements);

			return true;

		} else {

			return false;

		}
	}

	/**
	 * Create a answer message with exception.
	 *
	 */
	protected static OtpErlangTuple getAnswer(Exception e) {

		/* Init header tuple for answer:
		 * "java_message" + (request_type + request_result[0])
		 *
		 */
		OtpErlangTuple tuple_h = new OtpErlangTuple(new OtpErlangObject[]
				{new OtpErlangAtom("exception_raised"),
				 new OtpErlangAtom(e.getMessage())});

		OtpErlangTuple tuple_header = new OtpErlangTuple(new OtpErlangObject[]
			{new OtpErlangAtom("java_message"), tuple_h});

		// Initialises body tuple for answer:
		OtpErlangTuple tuple_stack = new OtpErlangTuple(new OtpErlangObject[]
				{new OtpErlangList(e.getStackTrace().toString())});

		OtpErlangTuple tuple_answer = new OtpErlangTuple(new OtpErlangObject[]
				{tuple_stack, tuple_header});

		return tuple_answer;

	}
}
