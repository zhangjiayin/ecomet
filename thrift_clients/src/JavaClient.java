
import org.apache.thrift.TException;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TSSLTransportFactory.TSSLTransportParameters;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;

import etao.erouter.EcometRouter;

public class JavaClient {
  public static void main(String [] args) {

  

    try {
      TTransport transport;
      
        transport = new TSocket("localhost", 9999);
        transport.open();
      
        /*
         * Similar to the server, you can use the parameters to setup client parameters or
         * use the default settings. On the client side, you will need a TrustStore which
         * contains the trusted certificate along with the public key. 
         * For this example it's a self-signed cert. 
         */
        //TSSLTransportParameters params = new TSSLTransportParameters();
        //params.setTrustStore("../../lib/java/test/.truststore", "thrift", "SunX509", "JKS");
        /*
         * Get a client transport instead of a server transport. The connection is opened on
         * invocation of the factory method, no need to specifically call open()
         */
        //transport = TSSLTransportFactory.getClientSocket("localhost", 9091, 0, params);
      

      TProtocol protocol = new  TBinaryProtocol(transport);
      EcometRouter.Client client = new EcometRouter.Client(protocol);

      perform(client);

      transport.close();
    } catch (TException x) {
      x.printStackTrace();
    } 
  }

  private static void perform(EcometRouter.Client client) throws TException
  {
   
    try {
    	client.send("1", "1", "hello");
    } catch (Exception io) {
      System.out.println("Invalid operation: " + io.getStackTrace());
    }
  }
}
