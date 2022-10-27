import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

class Frame extends JFrame {

    private JButton btnClose  = new JButton("Stäng");
    private JButton btnAddress = new JButton("Hitta samåkare!");

    private JTextField txtAddress = new JTextField();
    private JTextField txtCoor = new JTextField();

    private JLabel lblA = new JLabel("Address:");

    private JLabel lblC = new JLabel("Antal samåkare:");

    private JLabel lblCA = new JLabel("Välj rätt adress:");

    String address[] = {"Adress 1","Adress 2","Adress 3","Adress 4"};

    private JComboBox cb = new JComboBox(address);


    public Frame(){
        setTitle("Samåkningsapp");
        setSize(400,200);
        setLocation(new Point(300,200));
        setLayout(null);
        setResizable(false);

        initComponent();
        initEvent();
    }

    private void initComponent(){

        btnClose.setBounds(220,130, 150,25);
        btnAddress.setBounds(220,100, 150,25);

        txtAddress.setBounds(200,10,100,20);
        txtCoor.setBounds(200,45,100,20);

        lblA.setBounds(80,10,100,20);
        lblC.setBounds(80,45,100,20);
        lblCA.setBounds(10,100,100,20);

        cb.setBounds(120, 100,90,20);

        add(btnClose);
        add(btnAddress);

        add(lblA);
        add(lblC);
        add(lblCA);

        add(txtAddress);
        add(txtCoor);
    }

    private void initEvent(){

        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e){
                System.exit(1);
            }
        });

        btnClose.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                btnCloseClick(e);
            }
        });

        btnAddress.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                btnAddressClick(e);
            }
        });
    }

    private void btnCloseClick(ActionEvent evt){
        System.exit(0);
    }

    private void btnAddressClick(ActionEvent evt){
        String x,z;
        try{
            x = txtAddress.getText();
            z = x + " kul";
            txtCoor.setText(z.toString());
            add(cb);
            setVisible(true);

        }catch(Exception e){
            System.out.println(e);
            JOptionPane.showMessageDialog(null,
                    e.toString(),
                    "Error",
                    JOptionPane.ERROR_MESSAGE);
        }
    }
}