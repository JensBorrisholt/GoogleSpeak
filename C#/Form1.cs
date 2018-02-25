using System;
using System.Windows.Forms;
using GoogleSpeak.GoogleSpeak;

namespace GoogleSpeak
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            var googleLina = GoogleSpeech.Instance;
            googleLina.Say("Her er en hest");
            googleLina.Say("og en hest mere");
            googleLina.Say("and yet an other one", "en");
        }
    }
}
