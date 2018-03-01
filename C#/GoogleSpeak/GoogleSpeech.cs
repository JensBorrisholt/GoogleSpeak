using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using System.Web;
using NAudio.Wave;

namespace GoogleSpeak.GoogleSpeak
{
    public class GoogleSpeech
    {
        private readonly Queue<string> _playlist = new Queue<string>();
        private IWavePlayer _player;
        private Mp3FileReader _mp3FileReader;
        private string _downloadFolder;
        private static readonly Lazy<GoogleSpeech> Lazy = new Lazy<GoogleSpeech>(() => new GoogleSpeech());
        private bool _playing;
        private void Play()
        {
            if (_playlist.Count < 1)
                return;
            _playing = true;

            _player = new WaveOutEvent();
            _player.PlaybackStopped += (sender, e) =>
            {
                _player?.Dispose();
                _mp3FileReader?.Dispose();
                if (_playlist.Any())
                    Play();
                else
                    OnPlaybackStopped(e);
            }; 

            _mp3FileReader = new Mp3FileReader(_playlist.Dequeue());
            _player.Init(_mp3FileReader);
            _player.Play();
        }

        protected virtual void OnPlaybackStopped(StoppedEventArgs e)
        {
            _playing = false;
            if (e.Exception != null)
                PlaybackStopped?.Invoke(this, e);
        }

        public Task<string> DownloadFile(string text, Language language)
        {
            string Action(object obj)
            {
                var element = (string)obj;
                var filename = DownloadFolder + CRC32.Hash(language.Code + " " + text) + ".mp3";
                var url = $"https://translate.googleapis.com/translate_tts?ie=UTF-8&q={HttpUtility.UrlEncode(element)}&tl={language.Code}&total=1&idx=0&textlen={text.Length}&client=gtx";

                if (!File.Exists(filename))
                    using (var webClient = new WebClient())
                        webClient.DownloadFile(url, filename);

                return filename;
            }

            return Task<string>.Factory.StartNew(Action, text);
        }

        protected List<Task<string>> DownloadFiles(Language language, params string[] text) => text?.Select(element => DownloadFile(element, language)).ToList();

        public event EventHandler<StoppedEventArgs> PlaybackStopped;
        public Language Language { get; set; } = Languages.FromCode("da");
        public string DownloadFolder
        {
            get => _downloadFolder;
            set
            {
                _downloadFolder = value;
                Directory.CreateDirectory(_downloadFolder);
            }
        }

        public void Say(string text, string languageCode) => Say(text, Languages.FromCode(languageCode));
        public void Say(string text, Language language = null)
        {
            if (language == null)
                language = Language;

            _playlist.Enqueue(DownloadFile(text, language).Result);

            if (!_playing)
                Play();
        }

        public GoogleSpeech(bool useCache = true)
        {
            UseCache = useCache;
            DownloadFolder = AppDomain.CurrentDomain.BaseDirectory + @"Cache\";
        }

        ~GoogleSpeech()
        {
            if (!UseCache)
                Directory.Delete(DownloadFolder);
        }

        public static GoogleSpeech Instance => Lazy.Value;
        public bool UseCache { get; set; }
    }
}
