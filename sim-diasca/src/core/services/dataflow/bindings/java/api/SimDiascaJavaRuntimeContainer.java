import com.ericsson.otp.erlang.*;


// For IOException:
import java.io.*;

// For List:
import java.util.*;


/* This is the Sim-Diasca Java runtime container, i.e. the sources of the Java
 * program that is spawned by the engine on a computing done so that Java-based
 * actors can take part to the simulation.
 *
 */




/* Implementation notes.
 *
 * For each instance of JVM:
 *
 * - 1 controller, main mbox will be created, to serve overall needs such as
 *   engine-level synchronisation and creation of objects
 *
 * - N worker mboxes, where N is the number of created threads (usually: one per
 *   CPU core) will be also created; each worker mbox will drive the
 *   interactions with the thead it is in charge of
 *
 */


/*
 * Java-side runtime container for the engine.
 *
 * Allows to run a JVM on a computing node and have it properly interconnected
 * with the core of the engine.
 *
 * See also, from our jinterface-testing branch, our EchoServer.java example.
 *
 */
public class SimDiascaJavaRuntimeContainer
{

	// The main, local, controller mailbox:
	static OtpMbox mainMbox = null;


	/* The PID of the Erlang-side binding peer (the node-local binding agent,
	 * namely class_JavaBindingAgent.erl):
	 *
	 */
	static OtpErlangPid bindingAgentPid = null ;


	/*
	 * Manages the Java-side settings (either the default ones, or the ones
	 * overridden from the command-line).
	 *
	 */
	private static class ContainerSettings {

		// Listing the defaults:

		// General maximal networked waiting (ex: for ping), in milliseconds:
		public long timeOut = 2000;

		/* The name of this Java-based node (suffixed, thus uniquified, by the
		 * hostname):
		 */
		public String thisNodeName = "sim_diasca_java_node";

		// No sensible defaults exist for these information:

		public String peerNodeName = "";
		public String cookieName = "";

		public int coreCount = 1;

		// Non-standard default EPMD port:
		public int epmdPort = 4506;

		/*
		 * Name of the Erlang process to handshake with, i.e. the local
		 * registration name of the Java Binding Agent:
		 *
		 * (see the java_binding_agent_name define in
		 * class_JavaBindingAgent.erl)
		 *
		 */
		public String targetRegisteredName = "sim_diasca_java_binding_agent";


		public ContainerSettings(String[] args) throws Exception
		{

			// Poor man's getopt:

			int argCount = args.length ;

			for (int i=0; i < argCount; i++)
			{

				switch ( args[i] ) {

				case "--epmd-port":
					i++;
					epmdPort = Integer.parseInt(args[i]);
					send_debug_message("Setting EPMD port to '" + epmdPort
					  + "'.");
					break;

				case "--cookie":
					i++;
					cookieName = args[i];
					send_debug_message("Setting cookie name to '" + cookieName
					  + "'.");
					break;

				case "--this-node-name":
					i++;
					thisNodeName = args[i];
					send_debug_message("Setting the local node name to '"
					  + thisNodeName
					  + "'.");
					break;

				case "--peer-node-name":
					i++;
					peerNodeName = args[i];
					send_debug_message("Setting the peer node name to '"
					  + peerNodeName + "'.");
					break;

				case "--core-count":
					i++;
					coreCount = Integer.parseInt(args[i]);
					send_debug_message("Setting the core count to " + coreCount
					  + ".");
					break;

				default:
					throw new Exception("Invalid argument: '" + args[i]
					  + "'" );

				}
			}

			if ( cookieName.isEmpty() )
				throw new Exception( "Internode cookie must be set." );

			if ( peerNodeName.isEmpty() )
				throw new Exception( "Peer node name must be set." );

		}

	}



	/*
	 * Stores an incoming request call.
	 *
	 */
	private static class RequestCall {


		public final OtpErlangAtom requestName;
		public final OtpErlangList parameters;
		public final OtpErlangPid sender;


		/*
		 * Creates an instance storing a request call.
		 *
		 * Note: the caller shall have checked that specified tuple is a triplet
		 * (i.e. is of arity 3).
		 %
		 */
		public RequestCall(OtpErlangTuple tuple) throws Exception
		{

			/* We have a request here ; expected (Erlang) structure:
			 * { request_name() :: atom(), request_parameters() :: list(),
			 *   sender_pid() :: pid() }
			 */

			OtpErlangObject first = tuple.elementAt(0);

			if (first instanceof OtpErlangAtom)
			{

				this.requestName = (OtpErlangAtom) first;

				//send_debug_message( "Received a call to method '"
				//  + this.requestName.atomValue() + "'." );

				OtpErlangObject second = tuple.elementAt(1);

				if (second instanceof OtpErlangList)
				{

					this.parameters = (OtpErlangList) second;

					//send_debug_message( "Parameters: "
					//+ this.parameters.toString() );

					OtpErlangObject third = tuple.elementAt(2);

					if (third instanceof OtpErlangPid)
					{

						this.sender = (OtpErlangPid) third;

						//send_debug_message( "Sender: "
						//+ this.sender.toString() );

					}
					else
					{

						error( "Request sender (third element) is not a PID: "
						  + third.toString() );

						throw new Exception( "Invalid request sender" ) ;

					}

				}
				else
				{

					error( "Request parameters are not a list: "
					  + second.toString() );

					throw new Exception( "Invalid request parameters" ) ;

				}

			}
			else
			{

				error( "Request name is not an atom: " + first.toString() );
				throw new Exception( "Invalid request name" ) ;

			}

			// All attributes checked and stored.

		}


		/*
		 * Executes the corresponding request call, and returns the result term
		 * that is to be sent back to the request caller.
		 *
		 */
		public OtpErlangObject execute() throws Exception {

			/* Here we use the requestName and parameters to trigger the right
			 * (Java) code, whose execution is to return an OtpErlangObject
			 * like:
			 *
			 */

			/*
			 * Just a random example, meant to be updated with the actual Java
			 * reflective code!
			 *
			 */
			int rawResult = 77;

			// To test how Java-side exceptions are handled:

			//Boolean raiseException = true;
			Boolean raiseException = false;

			if ( raiseException )
			{

				throw new Exception(
				  "This is a Java-originating test exception" );

			}
			else
			{

				// Normal case, we want to return here { test_result, Result }:
				OtpErlangObject[] holder = new OtpErlangObject[2];

				holder[0] = new OtpErlangAtom("test_result");
				holder[1] = new OtpErlangInt(rawResult);

				// The pair to return:
				return new OtpErlangTuple(holder);

			}

		}


		// Returns a textual representation of this request call.
		public String toString() {
			return "request '" + this.requestName + "', with parameters "
				+ this.parameters.toString() + ", sent by " + this.sender ;
		}

	}


	// Launcher of the Java runtime container.
	public static void main(String[] args) throws Exception
	{

		send_info_message("Initialising this Sim-Diasca Java runtime container.");

		ContainerSettings settings = new ContainerSettings( args );

		try
		{

			send_debug_message("Creating Java node '" + settings.thisNodeName
			  + "', using cookie '" + settings.cookieName + "' and "
			  + settings.coreCount + " core(s).");

			OtpNode javaNode ;

			try
			{

				OtpEpmd.useEpmdPort(settings.epmdPort);

				javaNode = new OtpNode(settings.thisNodeName);

			} catch(IOException e) {

				throw new Exception(e.toString()
				  + "; no EPMD daemon found apparently; port mistmatch?");

			}

			javaNode.setCookie(settings.cookieName);

			if (javaNode.ping(settings.peerNodeName, settings.timeOut)) {
				send_debug_message("Engine node '" + settings.peerNodeName
				  + "' found.");
			}
			else {
				error("Engine (Erlang) node '" + settings.peerNodeName
				  + "' not found.");
				throw new RuntimeException("Engine node not found");
			}

			// The main, overall controlling mbox:
			String mainMboxName = "controller-mailbox" ;

			send_debug_message("Creating the main controller mailbox '"
			  + mainMboxName + "'.");

			mainMbox = javaNode.createMbox(mainMboxName);

			String mboxBaseName = "worker-mailbox" ;

			// To force a later de-allocation of worker mboxes:
			{

				OtpErlangList workerMboxPids = createWorkerMailboxes(
				  settings.coreCount, mboxBaseName, javaNode);

				manageHandshake(mainMbox, workerMboxPids, settings.peerNodeName,
				  settings.targetRegisteredName);

				handleMessages(mainMbox);

			}

			mainMbox.close();

			javaNode.close();

		}
		catch (Exception e)
		{

			error( e.toString() );

		}

		send_debug_message("Terminating.");

	}


	public static void handleMessages(OtpMbox mbox)
	{

		OtpErlangPid from;

		Boolean run = true;

		try {

			while (run)
			{

				send_debug_message("Waiting for a message from the engine.");

				// Blocking:
				OtpErlangObject msg = mbox.receive();

				send_debug_message("Message received: " + msg.toString());

				if (msg instanceof OtpErlangTuple)
				{

					OtpErlangTuple tuple = (OtpErlangTuple) msg;

					int elemCount = tuple.arity() ;

					if ( elemCount == 3 )
					{

						RequestCall call = new RequestCall( tuple ) ;

						send_debug_message("Received request call to "
						  + call.toString());

						OtpErlangObject resultTerm;

						try
						{

							resultTerm = call.execute();

						}
						catch (Exception e)
						{

							String exceptionString = e.toString();

							/* Exception raised, notifying the Erlang part and
							 * continuing:
							 *
							 */
							error("Exception raised, forwarding to engine: "
							  + exceptionString);

							/* Sending { onJavaExceptionThrown,
							 *           [ ExceptionString ] }:
							 *
							 */

							OtpErlangObject[] holder = new OtpErlangObject[2];

							holder[0] = new OtpErlangAtom(
							  "onJavaExceptionThrown");

							OtpErlangString excepString =
								new OtpErlangString(exceptionString);

							holder[1] = new OtpErlangList(excepString);

							OtpErlangTuple exceptionPair =
								new OtpErlangTuple(holder);

							mbox.send(call.sender, exceptionPair);

							continue ;

						}

						send_debug_message(
						  "Sending the result of request execution "
						  + resultTerm.toString() + " to " + call.sender);

						mbox.send(call.sender, resultTerm);

					}
					else
					{

						// We could as well have define a Onecall class:
						if ( elemCount == 2 )
						{

							// We have a pair then:

							OtpErlangObject first = tuple.elementAt(0);

							if( first instanceof OtpErlangAtom)
							{

								OtpErlangAtom onewayName =
									(OtpErlangAtom) first;

								OtpErlangObject second = tuple.elementAt(1);

								if (second instanceof OtpErlangList)
								{

									OtpErlangList parameters =
										(OtpErlangList) second;

									send_warning_message( "Call to oneway '"
									  + onewayName + "' with parameters "
									  + second.toString() + " not handled" );

								}
								else
								{

									error( "Oneway parameters are not a list: "
									  + second.toString() );

									throw new Exception(
									  "Invalid oneway parameters" ) ;

								}

							}
							else
							{

								error( "Oneway name is not an atom: "
								  + first.toString() );

								throw new Exception( "Invalid oneway name" ) ;

							}

						}
						else
						{

							send_error_message( "Unexpected tuple received, "
							  + "of size " + elemCount + ": "
							  + tuple.toString() );

						}

					}

				}
				else if (msg instanceof OtpErlangAtom)
				{

					String onewayName = ((OtpErlangAtom) msg).atomValue();

					if ( onewayName.equals("terminate") )
					{

						send_info_message( "Requesting to terminate, hence "
						  + "stopping." );

						System.exit( 0 );

					}
					else
					{

						send_error_message( "Unexpected oneway received: "
						  + onewayName );

					}

				}

			}

		}
		catch (Exception e) {

			error("Internal exception raised: " + e.toString());

		}

	}


	// Creates the worker mailboxes.
	private static OtpErlangList createWorkerMailboxes( int count,
	  String baseName, OtpNode targetNode )
	{

		OtpErlangPid[] workerMboxes = new OtpErlangPid[count];

		String mboxName;
		OtpMbox mbox;

		for (int i=0; i<count; i++ )
		{

			mboxName = baseName + "-" + i;

			send_debug_message("Creating worker mailbox '" + mboxName + "'.");

			mbox = targetNode.createMbox(mboxName);

			workerMboxes[i] = mbox.self();

		}

		return new OtpErlangList(workerMboxes);

	}


	// Deletes the worker mailboxes (meaningless in Java).
	// private static void deleteWorkerMailboxes(OtpErlangList workerMboxes)
	// {

	//	send_debug_message("Deleting " + workerMboxes.arity()
	//	  + " worker mailboxes.");

	//	OtpMbox workerMbox;

	//	final Iterator i = workerMboxes.iterator();

	//	while (i.hasNext())
	//	{

	//		workerMbox = (OtpMbox) i.next();
	//		delete workerMbox ;

	//	}

	// }



	/* Manages the two-way handshake with the Erlang side, initiated by this
	 * (Java) side.
	 *
	 */
	private static void manageHandshake(OtpMbox mainMbox,
	  OtpErlangList workerMboxPids, String peerNodeName,
	  String targetRegisteredName) throws Exception
	{

		sendHandshakeInitiation(mainMbox, workerMboxPids, peerNodeName,
		  targetRegisteredName);

		receiveHandshakeFinalisation(mainMbox);

	}


	private static void sendHandshakeInitiation(OtpMbox mainMbox,
	  OtpErlangList workerMboxPids, String peerNodeName,
	  String targetRegisteredName)
	{

		/*
		 * Handshake triggered by this just launched Java runtime, in the form
		 * of a message interpreted as a oneway by the node-local
		 * JavaBindingAgent:
		 *
		 * { handshakeRequest, [ MainMboxPid, WorkerMboxPids ] }
		 *
		 */

		// The overall pair:
		OtpErlangObject[] handshakeHolder = new OtpErlangObject[2];

		handshakeHolder[0] = new OtpErlangAtom("handshakeRequest");

		// The parameters:
		OtpErlangObject[] handshakeParams = new OtpErlangObject[2];

		// So that the binding agent knows all relevant mailboxes:
		handshakeParams[0] = mainMbox.self();
		handshakeParams[1] = workerMboxPids;

		handshakeHolder[1] = new OtpErlangList(handshakeParams);

		// The final pair:
		OtpErlangTuple handshakeMsg = new OtpErlangTuple(handshakeHolder);

		send_debug_message("Sending a handshake initiation request to "
		  + "registered process '" + targetRegisteredName
		  + "' on node '" + peerNodeName + "', waiting for answer.");

		mainMbox.send(targetRegisteredName, peerNodeName, handshakeMsg);

	}


	private static void receiveHandshakeFinalisation(OtpMbox mainMbox)
		throws Exception
	{

		/* The controller mbox expects to receive { handshakeConfirmed,
		 * BindingAgentPid } as an answer:
		 */
		OtpErlangObject handshake = mainMbox.receive();

		if (handshake instanceof OtpErlangTuple)
		{

			OtpErlangTuple tuple = (OtpErlangTuple) handshake;

			if ( tuple.arity() == 2 )
			{

				OtpErlangObject first = tuple.elementAt(0);

				if (first instanceof OtpErlangAtom)
				{
					OtpErlangAtom atom = (OtpErlangAtom) first;

					if (atom.atomValue().equals("handshakeConfirmed"))
					{

						OtpErlangObject second = tuple.elementAt(1);

						if (second instanceof OtpErlangPid)
						{

							// Last console output:
							send_info_message("Handshake completed, "
							  + "last console trace to be output, switching to "
							  + "networked ones now.");

							SimDiascaJavaRuntimeContainer.bindingAgentPid =
								(OtpErlangPid) second;

							send_debug_message("Handshake completed, the PID "
							  + "of the associated binding agent is "
							  + bindingAgentPid + ".");

						}
						else
						{
							throw new Exception("Peer element is not a PID") ;
						}

					}
					else
					{
						throw new Exception( "Handshake answer does not "
						  + "include the right atom, got: '"
						  + atom.atomValue() + "'" );

					}
				}
				else
				{
					throw new Exception(
					  "Handshake answer does not include an atom" );
				}

			}
			else
			{
				throw new Exception( "Handshake answer is not a pair" );
			}

		}
		else
		{
			throw new Exception( "Handshake answer is not a tuple" );
		}

		// Handshake over, ready for action!

	}



	// Sending a debug message to the engine.
	private static void send_debug_message(String message)
	{

		if ( mainMbox != null && bindingAgentPid != null )
		{

			// Sending { onJavaDebugMessage, [ Message ] } to the binding agent:

			OtpErlangObject[] holder = new OtpErlangObject[2];

			holder[0] = new OtpErlangAtom("onJavaDebugMessage");

			OtpErlangString messageString = new OtpErlangString(message);

			holder[1] = new OtpErlangList(messageString);

			OtpErlangTuple traceTuple = new OtpErlangTuple(holder);

			mainMbox.send(bindingAgentPid, traceTuple);

		}
		else
		{

			debug("[local-only] " + message);

		}

	}


	// Sending a trace message to the engine.
	private static void send_trace_message(String message)
	{

		if ( mainMbox != null && bindingAgentPid != null  )
		{

			// Sending { onJavaTraceMessage, [ Message ] } to the binding agent:

			OtpErlangObject[] holder = new OtpErlangObject[2];

			holder[0] = new OtpErlangAtom("onJavaTraceMessage");

			OtpErlangString messageString = new OtpErlangString(message);

			holder[1] = new OtpErlangList(messageString);

			OtpErlangTuple traceTuple = new OtpErlangTuple(holder);

			mainMbox.send(bindingAgentPid, traceTuple);

		}
		else
		{

			trace("[local-only] " + message);

		}

	}


	// Sending a info message to the engine.
	private static void send_info_message(String message)
	{

		if ( mainMbox != null && bindingAgentPid != null  )
		{

			// Sending { onJavaInfoMessage, [ Message ] } to the binding agent:

			OtpErlangObject[] holder = new OtpErlangObject[2];

			holder[0] = new OtpErlangAtom("onJavaInfoMessage");

			OtpErlangString messageString = new OtpErlangString(message);

			holder[1] = new OtpErlangList(messageString);

			OtpErlangTuple traceTuple = new OtpErlangTuple(holder);

			mainMbox.send(bindingAgentPid, traceTuple);

		}
		else
		{

			info("[local-only] " + message);

		}

	}


	// Sending a warning message to the engine.
	private static void send_warning_message(String message)
	{

		if ( mainMbox != null && bindingAgentPid != null  )
		{

			/* Sending { onJavaWarningMessage, [ Message ] } to the binding
			 * agent:
			 */

			OtpErlangObject[] holder = new OtpErlangObject[2];

			holder[0] = new OtpErlangAtom("onJavaWarningMessage");

			OtpErlangString messageString = new OtpErlangString(message);

			holder[1] = new OtpErlangList(messageString);

			OtpErlangTuple traceTuple = new OtpErlangTuple(holder);

			mainMbox.send(bindingAgentPid, traceTuple);

		}
		else
		{

			warning("[local-only] " + message);

		}

	}


	// Sending a error message to the engine.
	private static void send_error_message(String message)
	{

		if ( mainMbox != null && bindingAgentPid != null  )
		{

			// Sending { onJavaErrorMessage, [ Message ] } to the binding agent:

			OtpErlangObject[] holder = new OtpErlangObject[2];

			holder[0] = new OtpErlangAtom("onJavaErrorMessage");

			OtpErlangString messageString = new OtpErlangString(message);

			holder[1] = new OtpErlangList(messageString);

			OtpErlangTuple traceTuple = new OtpErlangTuple(holder);

			mainMbox.send(bindingAgentPid, traceTuple);

		}
		else
		{

			error("[local-only] " + message);

		}

	}



	// Sending a fatal message to the engine.
	private static void send_fatal_message(String message)
	{

		if ( mainMbox != null && bindingAgentPid != null  )
		{

			// Sending { onJavaFatalMessage, [ Message ] } to the binding agent:

			OtpErlangObject[] holder = new OtpErlangObject[2];

			holder[0] = new OtpErlangAtom("onJavaFatalMessage");

			OtpErlangString messageString = new OtpErlangString(message);

			holder[1] = new OtpErlangList(messageString);

			OtpErlangTuple traceTuple = new OtpErlangTuple(holder);

			mainMbox.send(bindingAgentPid, traceTuple);

		}
		else
		{

			fatal("[local-only] " + message);

		}

	}



	// Local, console-based debug tracing.
	private static void debug(String message)
	{

		System.out.println("[debug] " + message );

	}


	// Local, console-based trace tracing.
	private static void trace(String message)
	{

		System.out.println("[trace] " + message );

	}


	// Local, console-based info tracing.
	private static void info(String message)
	{

		System.out.println("[info] " + message );

	}


	// Local, console-based warning tracing.
	private static void warning(String message)
	{

		System.out.println("[warning] " + message );

	}


	// Local, console-based error tracing.
	private static void error(String message)
	{

		System.out.println("[error] " + message );

	}


	// Local, console-based fatal tracing.
	private static void fatal(String message)
	{

		System.out.println("[fatal] " + message );

	}


}
