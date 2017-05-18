package tp6;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

/*********
Serv2IHM :
**********
Cette classe permet de g�rer l'IHM d'un Serveur IP (ou de tout autre objet
Serveur fonctionnant dans le cadre d'une boucle d'attente infinie)

  Fonctions :
  ----------
    1 - Fournir un bouton permettant de stopper le Serveur (qui sinon attend
    de nouvelles requ�tes dans une boucle infinie).
    2 - Fournir au Serveur un panneau permettant d'afficher du texte � l'aide
    de la m�thode "affiche(string message)"
    3 - Fournir une zone de saisie de texte  permettant de d�clancher
    une action sur le Serveur avec le texte saisie comme param�tre.

  Utilisation :
  -------------
    Un objet de la classe Serv2IHM doit �tre cr�� avant le lancement du serveur.
    La cr�ation de cet objet permet d'afficher une fen�tre proposant le bouton
    d'arr�t, le panneau d'afffichage et la zone de saisie de texte.

    Une r�f�rence sur l'IHM doit �tre fournie au Serveur pour lui permettre
    d'afficher des messages.

    Pour pouvoir r�agir aux actions d�clanch�es par la zone de saisie, la
    classe Serveur doit impl�menter l'interface Serv2ActionListener (et donc
    impl�menter la m�thode public void actionPerformed(String data); qui est
    contenue dans cette interface).La classe Serveur doit aussi se d�clarer
    en tant que r�cepteur d'�v�nement aupr�s de la classe Serv2IHM grace � la
    m�thode public void addServ2ActionListener(Serv2ActionListener l). Il est
    possible d'ajouter plusieurs r�cepteurs d'�v�nements.

    Exemple :
      {
        Serv2IHM fen = new Serv2IHM("Descripion du serveur");
        new Serveur(fen);
      }

    Pour afficher un message sur la fen�tre de l'IHM il suffit d'utiliser la
    m�thode "affiche(String message)" dans les m�thodes de la classe Serveur.
    Exemple :
      fen.affiche("contenu du texte � afficher");

    Pour pouvoir r�agir aux entr�es de la zone de saisie, la classe Serveur
    doit h�riter de l'interface Serv2ActionListener et se signaler comme
    �couteur d'�v�nement.
    Exemple :
      fen.addServ2ActionListener(this);
********/


public class Serv2IHM extends Frame {
  // attributs de l'IHM :
  Panel panel1 = new Panel();                       // Fen�tre principale
  BorderLayout borderLayout1 = new BorderLayout();  // Gestionnaire d'aspect
  Button bArret = new Button();                     // Bouton d'arr�t du serveur
  Label texte = new Label();                        // Affichage du nom
  Panel panel2 = new Panel();                       // Panneau d'affichage
  ScrollPane scrollPane1 = new ScrollPane();
  TextArea messages = new TextArea();               // Zone d'affichage
  Label blanc = new Label();
  Panel panel3 = new Panel();                       // Panneau de Saisie
  TextField messageText = new TextField();          // Zone de Saisie

  Vector recepteurs = new Vector(); // liste des recepteurs d'�v�nements

  // M�thode permettant d'afficher un message dans la zone texte
  public void affiche(String mess) {
    messages.append(mess+"\n" );
  }

  // M�thode permettant d'ajouter un r�cepteur d'�v�nement
  public void addServ2ActionListener(Serv2ActionListener e) {
    recepteurs.addElement(e);
  }

  // Construction
  public Serv2IHM( String title) {
    super(title);
    try  {
      jbInit();
      texte.setText(title);
      pack();
      show();
    }
    catch (Exception ex) {
      ex.printStackTrace();
    }
  }

  public Serv2IHM() {
    this("SERVEUR");
  }

  // Initialisation de l'IHM
  void jbInit() throws Exception {
    panel1.setLayout(borderLayout1);
    bArret.setLabel("Arret !");
    bArret.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        bArret_actionPerformed(e);
      }
    });
    this.addWindowListener(new java.awt.event.WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        this_windowClosing(e);
      }
    });
    messageText.setColumns(50);
    messageText.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        messageText_actionPerformed(e);
      }
    });
    blanc.setText("                                                               ");
    this.setTitle("                         ");
    this.add(panel1, BorderLayout.CENTER);
    panel1.add(panel2, BorderLayout.NORTH);
    panel2.add(texte, null);
    panel2.add(bArret, null);
    panel2.add(blanc, null);
    panel1.add(scrollPane1, BorderLayout.CENTER);
    scrollPane1.add(messages, null);
    panel1.add(panel3,BorderLayout.SOUTH);
    panel3.add(messageText, null);
  }

  // ACTION du bouton stop : arr�t forc� de l'application
  void bArret_actionPerformed(ActionEvent e) {
    System.exit(1);
  }
  // Idem pour la fermeture forc�e de la fen�tre
  void this_windowClosing(WindowEvent e) {
    System.exit(1);
  }
  // Idem pour la saisie d'un texte dans la zone de saisie
  void messageText_actionPerformed(ActionEvent e) {
    for (Enumeration enumere = recepteurs.elements(); enumere.hasMoreElements();)
      ((Serv2ActionListener)enumere.nextElement()).actionPerformed(messageText.getText());
    messageText.setText("");
  }
}


