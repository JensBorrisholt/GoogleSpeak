unit LanguagesU;

interface

uses
  System.Generics.Collections;

type
  TLanguage = class
    Code: string;
    Name: string;
  private
    constructor Create(aName, aCode: string);
  end;

  TLanguages = class
  private
    class var FList: TObjectList<TLanguage>;
  public
    class constructor Create;
    class destructor Destroy;
    class property List: TObjectList<TLanguage> read FList;
    class function FromCode(aCode: String): TLanguage;
    class function FromName(aName: String): TLanguage;
  end;

implementation

{ TLanguages }

class constructor TLanguages.Create;
begin
  FList := TObjectList<TLanguage>.Create;
  FList.Add(TLanguage.Create('Afrikaans', 'af'));
  FList.Add(TLanguage.Create('Albanian', 'sq'));
  FList.Add(TLanguage.Create('Amharic', 'am'));
  FList.Add(TLanguage.Create('Arabic', 'ar'));
  FList.Add(TLanguage.Create('Armenian', 'hy'));
  FList.Add(TLanguage.Create('Azeerbaijani', 'az'));
  FList.Add(TLanguage.Create('Basque', 'eu'));
  FList.Add(TLanguage.Create('Belarusian', 'be'));
  FList.Add(TLanguage.Create('Bengali', 'bn'));
  FList.Add(TLanguage.Create('Bosnian', 'bs'));
  FList.Add(TLanguage.Create('Bulgarian', 'bg'));
  FList.Add(TLanguage.Create('Catalan', 'ca'));
  FList.Add(TLanguage.Create('Cebuano', 'ceb (ISO-639-2)'));
  FList.Add(TLanguage.Create('Chinese (Simplified)', 'zh-CN'));
  FList.Add(TLanguage.Create('Chinese (Traditional)', 'zh-TW'));
  FList.Add(TLanguage.Create('Corsican', 'co'));
  FList.Add(TLanguage.Create('Croatian', 'hr'));
  FList.Add(TLanguage.Create('Czech', 'cs'));
  FList.Add(TLanguage.Create('Danish', 'da'));
  FList.Add(TLanguage.Create('Dutch', 'nl'));
  FList.Add(TLanguage.Create('English', 'en'));
  FList.Add(TLanguage.Create('Esperanto', 'eo'));
  FList.Add(TLanguage.Create('Estonian', 'et'));
  FList.Add(TLanguage.Create('Finnish', 'fi'));
  FList.Add(TLanguage.Create('French', 'fr'));
  FList.Add(TLanguage.Create('Frisian', 'fy'));
  FList.Add(TLanguage.Create('Galician', 'gl'));
  FList.Add(TLanguage.Create('Georgian', 'ka'));
  FList.Add(TLanguage.Create('German', 'de'));
  FList.Add(TLanguage.Create('Greek', 'el'));
  FList.Add(TLanguage.Create('Gujarati', 'gu'));
  FList.Add(TLanguage.Create('Haitian Creole', 'ht'));
  FList.Add(TLanguage.Create('Hausa', 'ha'));
  FList.Add(TLanguage.Create('Hawaiian', 'haw'));
  FList.Add(TLanguage.Create('Hebrew', 'iw'));
  FList.Add(TLanguage.Create('Hindi', 'hi'));
  FList.Add(TLanguage.Create('Hmong', 'hmn'));
  FList.Add(TLanguage.Create('Hungarian', 'hu'));
  FList.Add(TLanguage.Create('Icelandic', 'is'));
  FList.Add(TLanguage.Create('Igbo', 'ig'));
  FList.Add(TLanguage.Create('Indonesian', 'id'));
  FList.Add(TLanguage.Create('Irish', 'ga'));
  FList.Add(TLanguage.Create('Italian', 'it'));
  FList.Add(TLanguage.Create('Japanese', 'ja'));
  FList.Add(TLanguage.Create('Javanese', 'jw'));
  FList.Add(TLanguage.Create('Kannada', 'kn'));
  FList.Add(TLanguage.Create('Kazakh', 'kk'));
  FList.Add(TLanguage.Create('Khmer', 'km'));
  FList.Add(TLanguage.Create('Korean', 'ko'));
  FList.Add(TLanguage.Create('Kurdish', 'ku'));
  FList.Add(TLanguage.Create('Kyrgyz', 'ky'));
  FList.Add(TLanguage.Create('Lao', 'lo'));
  FList.Add(TLanguage.Create('Latin', 'la'));
  FList.Add(TLanguage.Create('Latvian', 'lv'));
  FList.Add(TLanguage.Create('Lithuanian', 'lt'));
  FList.Add(TLanguage.Create('Luxembourgish', 'lb'));
  FList.Add(TLanguage.Create('Macedonian', 'mk'));
  FList.Add(TLanguage.Create('Malagasy', 'mg'));
  FList.Add(TLanguage.Create('Malay', 'ms'));
  FList.Add(TLanguage.Create('Malayalam', 'ml'));
  FList.Add(TLanguage.Create('Maltese', 'mt'));
  FList.Add(TLanguage.Create('Maori', 'mi'));
  FList.Add(TLanguage.Create('Marathi', 'mr'));
  FList.Add(TLanguage.Create('Mongolian', 'mn'));
  FList.Add(TLanguage.Create('Myanmar (Burmese)', 'my'));
  FList.Add(TLanguage.Create('Nepali', 'ne'));
  FList.Add(TLanguage.Create('Norwegian', 'no'));
  FList.Add(TLanguage.Create('Nyanja (Chichewa)', 'ny'));
  FList.Add(TLanguage.Create('Pashto', 'ps'));
  FList.Add(TLanguage.Create('Persian', 'fa'));
  FList.Add(TLanguage.Create('Polish', 'pl'));
  FList.Add(TLanguage.Create('Portuguese (Portugal, Brazil)', 'pt'));
  FList.Add(TLanguage.Create('Punjabi', 'pa'));
  FList.Add(TLanguage.Create('Romanian', 'ro'));
  FList.Add(TLanguage.Create('Russian', 'ru'));
  FList.Add(TLanguage.Create('Samoan', 'sm'));
  FList.Add(TLanguage.Create('Scots Gaelic', 'gd'));
  FList.Add(TLanguage.Create('Serbian', 'sr'));
  FList.Add(TLanguage.Create('Sesotho', 'st'));
  FList.Add(TLanguage.Create('Shona', 'sn'));
  FList.Add(TLanguage.Create('Sindhi', 'sd'));
  FList.Add(TLanguage.Create('Sinhala (Sinhalese)', 'si'));
  FList.Add(TLanguage.Create('Slovak', 'sk'));
  FList.Add(TLanguage.Create('Slovenian', 'sl'));
  FList.Add(TLanguage.Create('Somali', 'so'));
  FList.Add(TLanguage.Create('Spanish', 'es'));
  FList.Add(TLanguage.Create('Sundanese', 'su'));
  FList.Add(TLanguage.Create('Swahili', 'sw'));
  FList.Add(TLanguage.Create('Swedish', 'sv'));
  FList.Add(TLanguage.Create('Tagalog (Filipino)', 'tl'));
  FList.Add(TLanguage.Create('Tajik', 'tg'));
  FList.Add(TLanguage.Create('Tamil', 'ta'));
  FList.Add(TLanguage.Create('Telugu', 'te'));
  FList.Add(TLanguage.Create('Thai', 'th'));
  FList.Add(TLanguage.Create('Turkish', 'tr'));
  FList.Add(TLanguage.Create('Ukrainian', 'uk'));
  FList.Add(TLanguage.Create('Urdu', 'ur'));
  FList.Add(TLanguage.Create('Uzbek', 'uz'));
  FList.Add(TLanguage.Create('Vietnamese', 'vi'));
  FList.Add(TLanguage.Create('Welsh', 'cy'));
  FList.Add(TLanguage.Create('Xhosa', 'xh'));
  FList.Add(TLanguage.Create('Yiddish', 'yi'));
  FList.Add(TLanguage.Create('Yoruba', 'yo'));
  FList.Add(TLanguage.Create('Zulu', 'zu'));
end;

class destructor TLanguages.Destroy;
begin
  FList.Free;
end;

class function TLanguages.FromCode(aCode: String): TLanguage;
var
  Language: TLanguage;
begin
  for Language in FList do
    if Language.Code = aCode then
      exit(Language);

  exit(nil);
end;

class function TLanguages.FromName(aName: String): TLanguage;
var
  Language: TLanguage;
begin
  for Language in FList do
    if Language.Name = aName then
      exit(Language);

  exit(nil);
end;

{ TLanguage }

constructor TLanguage.Create(aName, aCode: string);
begin
  Code := aCode;
  Name := aName;
end;

end.
