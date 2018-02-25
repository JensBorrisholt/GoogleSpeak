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
        private readonly Queue<string> playlist = new Queue<string>();
        private IWavePlayer player;
        private Mp3FileReader mp3FileReader;
        private string downloadFolder;
        private static readonly Lazy<GoogleSpeech> Lazy = new Lazy<GoogleSpeech>(() => new GoogleSpeech());
        private bool playing;

        public event EventHandler<StoppedEventArgs> PlaybackStopped;
        public Language Language { get; set; } = Languages.FromCode("da");

        private void Play()
        {
            if (playlist.Count < 1)
                return;
            playing = true;

            player = new WaveOutEvent();
            player.PlaybackStopped += (sender, e) =>
            {
                player?.Dispose();
                mp3FileReader?.Dispose();
                if (playlist.Any())
                    Play();
                else
                    OnPlaybackStopped(e);
            };

            mp3FileReader = new Mp3FileReader(playlist.Dequeue());
            player.Init(mp3FileReader);
            player.Play();
        }

        protected virtual void OnPlaybackStopped(StoppedEventArgs e)
        {
            playing = false;
            PlaybackStopped?.Invoke(this, e);
        }


        public void Say(string text, string languageCode)
        {
            var language = Languages.FromCode(languageCode);
            Say(text, language);
        }

        public void Say(string text, Language language = null)
        {
            if (language == null)
                language = Language;

            var filename = DownloadFolder + CRC32.Hash(language.Code + " " + text) + ".mp3";
            var url = $"https://translate.googleapis.com/translate_tts?ie=UTF-8&q={HttpUtility.UrlEncode(text)}&tl={language.Code}&total=1&idx=0&textlen={text.Length}&client=gtx";

            if (!UseCache || !File.Exists(filename))
                using (var webClient = new WebClient())
                    webClient.DownloadFile(url, filename);

            playlist.Enqueue(filename);

            if (!playing)
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

        public string DownloadFolder
        {
            get => downloadFolder;
            set
            {
                downloadFolder = value;
                Directory.CreateDirectory(downloadFolder);
            }
        }

        public static GoogleSpeech Instance => Lazy.Value;
        public bool UseCache { get; set; }
    }
}
