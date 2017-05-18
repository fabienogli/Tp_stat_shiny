package tp6;

/**
 */
import java.net.*;

public class Discussion implements Serv2ActionListener, Runnable {

  private Serv2IHM fen ;         // IHM
  final static int MAX_DATA_SIZE = 128;  // Taille maximale des donnees envoyees

  InetAddress adresse;           // Adresse Internet du groupe de multicast UDP
  int port;                      // Port UDP du groupe de multicast UDP
  int ttl;                       // parametre : time to live
  MulticastSocket socket;// Socket multicast
  DatagramSocket socket2;


  // Constructeur
  public Discussion(Serv2IHM fen,InetAddress adresse
                    ,int port, int ttl) throws Exception {
    this.fen = fen;
    this.adresse=adresse;
    this.port=port;
    this.ttl=ttl;
    fen.addServ2ActionListener(this); // Association e l'IHM pour les saisies
    // Initialisation du Socket multicast, de son ttl, et association e
     socket = new MulticastSocket(port);
    // l'adresse de multicasting
    //adresse = InetAddress.getByName("225.1.1.1") ;
    socket.joinGroup(adresse);
    socket.setTimeToLive(ttl);
    ttl = 1;
    // Fin Initialisation
    // Creation et demarrage d'un processus leger  qui va executer
    // la methode run pour gerer la reception et l'affichage
    new Thread(this).start();
  }

  // Gestion de la reception des donnees de l'IHM et envoie vers le reseau
  public void actionPerformed(String mess) {
    try {
      // Gerer ici l'emission de la donnee mess en multicast
    	DatagramPacket dp = new DatagramPacket( mess.getBytes(),
    			mess.length(), adresse, port ) ;
    	socket.send(dp) ;
    	//socket.close();
      // Fin de l'emission
  } catch(Exception e){ }
  }

  // Gestion de la reception des donnees du reseau et affichage vers l'IHM
  public void run() {
    try {
      // Afficher en permanence ce que l'on reoit en multicast vers l'IHM
      // (utilisation de fen.affiche(...) dans une boucle infinie
    	while (true) {
    		DatagramPacket msg = new DatagramPacket( new byte[MAX_DATA_SIZE], MAX_DATA_SIZE  ) ;
    		socket.receive(msg) ;
                //socket2.receive(msg) ;
    		//affiche dans la fenetre l'adresse de l'expediteur, le port, et le message
    		fen.affiche(msg.getAddress() + ":" + msg.getPort()
    		+ " a envoyé " + new String(msg.getData(),msg.getOffset(),msg.getLength())) ;
    		}
      // Fin reception
    } catch(Exception e){ }
  }

  // Programme principal
  public static void main(String[] args) throws Exception {
	  //initialisation des variables
    String adresse = "224.4.4.4";
    int port = 5000;
    int ttl=1;/*
    //si le nombre d'arguments est different de 0
    if(args.length != 0) {
    	//si le nombre d'arguments est different de 3
      if (args.length!=3) {
    	  //on affiche ceci
        System.out.println("Usage: java MulticastDiscussion [addr port ttl]");
        return;
      }
      //sinon on execute ce qui suit
      else {
    	  //on recupere le 1er argument qui est l'adresse 
        adresse = args[0];
      //on recupere le 2eme argument qui est le port
        port = Integer.parseInt(args[1]);
      //on recupere le 3eme argument qui est le ttl
        ttl = Integer.parseInt(args[2]);
      }
    }*/
    Serv2IHM fen = new Serv2IHM("Discussion");
      Discussion discussion = new Discussion(fen, InetAddress.getByName(adresse), port, ttl);
  }
}