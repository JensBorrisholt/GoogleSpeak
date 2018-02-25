using System.Collections.Generic;
using System.Linq;

namespace GoogleSpeak.GoogleSpeak
{
    public class Language
    {
        public string Code { get; set; }
        public string Name { get; set; }
    }

    public static class Languages
    {
        public static List<Language> List { get; }

        public static Language FromCode(string code) => List.FirstOrDefault(e => e.Code == code);
        public static Language FromName(string name) => List.FirstOrDefault(e => e.Name == name);

        static Languages()
        {
            List = new List<Language>
            {
                new Language {Name = "Afrikaans", Code = "af"},
                new Language {Name = "Albanian", Code = "sq"},
                new Language {Name = "Amharic", Code = "am"},
                new Language {Name = "Arabic", Code = "ar"},
                new Language {Name = "Armenian", Code = "hy"},
                new Language {Name = "Azeerbaijani", Code = "az"},
                new Language {Name = "Basque", Code = "eu"},
                new Language {Name = "Belarusian", Code = "be"},
                new Language {Name = "Bengali", Code = "bn"},
                new Language {Name = "Bosnian", Code = "bs"},
                new Language {Name = "Bulgarian", Code = "bg"},
                new Language {Name = "Catalan", Code = "ca"},
                new Language {Name = "Cebuano", Code = "ceb (ISO-639-2)"},
                new Language {Name = "Chinese (Simplified)", Code = "zh-CN"},
                new Language {Name = "Chinese (Traditional)", Code = "zh-TW"},
                new Language {Name = "Corsican", Code = "co"},
                new Language {Name = "Croatian", Code = "hr"},
                new Language {Name = "Czech", Code = "cs"},
                new Language {Name = "Danish", Code = "da"},
                new Language {Name = "Dutch", Code = "nl"},
                new Language {Name = "English", Code = "en"},
                new Language {Name = "Esperanto", Code = "eo"},
                new Language {Name = "Estonian", Code = "et"},
                new Language {Name = "Finnish", Code = "fi"},
                new Language {Name = "French", Code = "fr"},
                new Language {Name = "Frisian", Code = "fy"},
                new Language {Name = "Galician", Code = "gl"},
                new Language {Name = "Georgian", Code = "ka"},
                new Language {Name = "German", Code = "de"},
                new Language {Name = "Greek", Code = "el"},
                new Language {Name = "Gujarati", Code = "gu"},
                new Language {Name = "Haitian Creole", Code = "ht"},
                new Language {Name = "Hausa", Code = "ha"},
                new Language {Name = "Hawaiian", Code = "haw"},
                new Language {Name = "Hebrew", Code = "iw"},
                new Language {Name = "Hindi", Code = "hi"},
                new Language {Name = "Hmong", Code = "hmn"},
                new Language {Name = "Hungarian", Code = "hu"},
                new Language {Name = "Icelandic", Code = "is"},
                new Language {Name = "Igbo", Code = "ig"},
                new Language {Name = "Indonesian", Code = "id"},
                new Language {Name = "Irish", Code = "ga"},
                new Language {Name = "Italian", Code = "it"},
                new Language {Name = "Japanese", Code = "ja"},
                new Language {Name = "Javanese", Code = "jw"},
                new Language {Name = "Kannada", Code = "kn"},
                new Language {Name = "Kazakh", Code = "kk"},
                new Language {Name = "Khmer", Code = "km"},
                new Language {Name = "Korean", Code = "ko"},
                new Language {Name = "Kurdish", Code = "ku"},
                new Language {Name = "Kyrgyz", Code = "ky"},
                new Language {Name = "Lao", Code = "lo"},
                new Language {Name = "Latin", Code = "la"},
                new Language {Name = "Latvian", Code = "lv"},
                new Language {Name = "Lithuanian", Code = "lt"},
                new Language {Name = "Luxembourgish", Code = "lb"},
                new Language {Name = "Macedonian", Code = "mk"},
                new Language {Name = "Malagasy", Code = "mg"},
                new Language {Name = "Malay", Code = "ms"},
                new Language {Name = "Malayalam", Code = "ml"},
                new Language {Name = "Maltese", Code = "mt"},
                new Language {Name = "Maori", Code = "mi"},
                new Language {Name = "Marathi", Code = "mr"},
                new Language {Name = "Mongolian", Code = "mn"},
                new Language {Name = "Myanmar (Burmese)", Code = "my"},
                new Language {Name = "Nepali", Code = "ne"},
                new Language {Name = "Norwegian", Code = "no"},
                new Language {Name = "Nyanja (Chichewa)", Code = "ny"},
                new Language {Name = "Pashto", Code = "ps"},
                new Language {Name = "Persian", Code = "fa"},
                new Language {Name = "Polish", Code = "pl"},
                new Language {Name = "Portuguese (Portugal, Brazil)", Code = "pt"},
                new Language {Name = "Punjabi", Code = "pa"},
                new Language {Name = "Romanian", Code = "ro"},
                new Language {Name = "Russian", Code = "ru"},
                new Language {Name = "Samoan", Code = "sm"},
                new Language {Name = "Scots Gaelic", Code = "gd"},
                new Language {Name = "Serbian", Code = "sr"},
                new Language {Name = "Sesotho", Code = "st"},
                new Language {Name = "Shona", Code = "sn"},
                new Language {Name = "Sindhi", Code = "sd"},
                new Language {Name = "Sinhala (Sinhalese)", Code = "si"},
                new Language {Name = "Slovak", Code = "sk"},
                new Language {Name = "Slovenian", Code = "sl"},
                new Language {Name = "Somali", Code = "so"},
                new Language {Name = "Spanish", Code = "es"},
                new Language {Name = "Sundanese", Code = "su"},
                new Language {Name = "Swahili", Code = "sw"},
                new Language {Name = "Swedish", Code = "sv"},
                new Language {Name = "Tagalog (Filipino)", Code = "tl"},
                new Language {Name = "Tajik", Code = "tg"},
                new Language {Name = "Tamil", Code = "ta"},
                new Language {Name = "Telugu", Code = "te"},
                new Language {Name = "Thai", Code = "th"},
                new Language {Name = "Turkish", Code = "tr"},
                new Language {Name = "Ukrainian", Code = "uk"},
                new Language {Name = "Urdu", Code = "ur"},
                new Language {Name = "Uzbek", Code = "uz"},
                new Language {Name = "Vietnamese", Code = "vi"},
                new Language {Name = "Welsh", Code = "cy"},
                new Language {Name = "Xhosa", Code = "xh"},
                new Language {Name = "Yiddish", Code = "yi"},
                new Language {Name = "Yoruba", Code = "yo"},
                new Language {Name = "Zulu", Code = "zu"}
            };
        }
    }
}
