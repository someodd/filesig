{-# LANGUAGE OverloadedStrings #-}
module Magic where

import Data.ByteString (ByteString)
import Data.HashMap.Strict as HM
import Data.Text (Text)

type ExtensionKey_ = Text
type Mime_ = Text
type Signature_ = (Int, ByteString)

rawMagicMap :: HM.HashMap ExtensionKey_ ([Signature_], Mime_)
rawMagicMap = HM.fromList [
    ("123",
        ( [ (0, "00001A00051004") ]
        , "application/vnd.lotus-1-2-3"
        )
    )
  , ("cpl",
        ( [ (0, "4D5A")
          , (0, "DCDC")
          ]
        , "application/cpl+xml"
        )
    )
  , ("epub",
        ( [ (0, "504B03040A000200") ]
        , "application/epub+zip"
        )
    )
  , ("ttf",
        ( [ (0, "0001000000") ]
        , "application/font-sfnt"
        )
    )
  , ("gz",
        ( [ (0, "1F8B08") ]
        , "application/gzip"
        )
    )
  , ("tgz",
        ( [ (0, "1F8B08") ]
        , "application/gzip"
        )
    )
  , ("hqx",
        ( [ (0, "28546869732066696C65206D75737420626520636F6E76657274656420776974682042696E48657820") ]
        , "application/mac-binhex40"
        )
    )
  , ("doc",
        ( [ (0, "0D444F43")
          , (0, "CF11E0A1B11AE100")
          , (0, "D0CF11E0A1B11AE1")
          , (0, "DBA52D00")
          , (512, "ECA5C100")
          ]
        , "application/msword"
        )
    )
  , ("mxf",
        ( [ (0, "060E2B34020501010D0102010102")
          , (0, "3C435472616E7354696D656C696E653E")
          ]
        , "application/mxf"
        )
    )
  , ("lha",
        ( [ (2, "2D6C68") ]
        , "application/octet-stream"
        )
    )
  , ("lzh",
        ( [ (2, "2D6C68") ]
        , "application/octet-stream"
        )
    )
  , ("exe",
        ( [ (0, "4D5A") ]
        , "application/octet-stream"
        )
    )
  , ("class",
        ( [ (0, "CAFEBABE") ]
        , "application/octet-stream"
        )
    )
  , ("dll",
        ( [ (0, "4D5A") ]
        , "application/octet-stream"
        )
    )
  , ("img",
        ( [ (0, "000100005374616E64617264204A6574204442")
          , (0, "504943540008")
          , (0, "514649FB")
          , (0, "53434D49")
          , (0, "7E742C015070024D52010000000800000001000031000000310000004301FF0001000800010000007e742c01")
          , (0, "EB3C902A")
          ]
        , "application/octet-stream"
        )
    )
  , ("iso",
        ( [ (32769, "4344303031")
          , (34817, "4344303031")
          , (36865, "4344303031")
          ]
        , "application/octet-stream"
        )
    )
  , ("ogx",
        ( [ (0, "4F67675300020000000000000000") ]
        , "application/ogg"
        )
    )
  , ("oxps",
        ( [ (0, "504B0304") ]
        , "application/oxps"
        )
    )
  , ("pdf",
        ( [ (0, "25504446") ]
        , "application/pdf"
        )
    )
  , ("p10",
        ( [ (0, "64000000") ]
        , "application/pkcs10"
        )
    )
  , ("pls",
        ( [ (0, "5B706C61796C6973745D") ]
        , "application/pls+xml"
        )
    )
  , ("eps",
        ( [ (0, "252150532D41646F62652D332E3020455053462D332030")
          , (0, "C5D0D3C6")
          ]
        , "application/postscript"
        )
    )
  , ("ai",
        ( [ (0, "25504446") ]
        , "application/postscript"
        )
    )
  , ("rtf",
        ( [ (0, "7B5C72746631") ]
        , "application/rtf"
        )
    )
  , ("tsa",
        ( [ (0, "47") ]
        , "application/tamp-sequence-adjust"
        )
    )
  , ("msf",
        ( [ (0, "2F2F203C212D2D203C6D64623A6D6F726B3A7A") ]
        , "application/vnd.epson.msf"
        )
    )
  , ("fdf",
        ( [ (0, "25504446") ]
        , "application/vnd.fdf"
        )
    )
  , ("fm",
        ( [ (0, "3C4D616B657246696C6520") ]
        , "application/vnd.framemaker"
        )
    )
  , ("kmz",
        ( [ (0, "504B0304") ]
        , "application/vnd.google-earth.kmz"
        )
    )
  , ("tpl",
        ( [ (0, "0020AF30")
          , (0, "6D7346696C7465724C697374")
          ]
        , "application/vnd.groove-tool-template"
        )
    )
  , ("kwd",
        ( [ (0, "504B0304") ]
        , "application/vnd.kde.kword"
        )
    )
  , ("wk4",
        ( [ (0, "00001A000210040000000000") ]
        , "application/vnd.lotus-1-2-3"
        )
    )
  , ("wk3",
        ( [ (0, "00001A000010040000000000") ]
        , "application/vnd.lotus-1-2-3"
        )
    )
  , ("wk1",
        ( [ (0, "0000020006040600080000000000") ]
        , "application/vnd.lotus-1-2-3"
        )
    )
  , ("apr",
        ( [ (0, "D0CF11E0A1B11AE1") ]
        , "application/vnd.lotus-approach"
        )
    )
  , ("nsf",
        ( [ (0, "1A0000040000")
          , (0, "4E45534D1A01")
          ]
        , "application/vnd.lotus-notes"
        )
    )
  , ("ntf",
        ( [ (0, "1A0000")
          , (0, "30314F52444E414E43452053555256455920202020202020")
          , (0, "4E49544630")
          ]
        , "application/vnd.lotus-notes"
        )
    )
  , ("org",
        ( [ (0, "414F4C564D313030") ]
        , "application/vnd.lotus-organizer"
        )
    )
  , ("lwp",
        ( [ (0, "576F726450726F") ]
        , "application/vnd.lotus-wordpro"
        )
    )
  , ("sam",
        ( [ (0, "5B50686F6E655D") ]
        , "application/vnd.lotus-wordpro"
        )
    )
  , ("mif",
        ( [ (0, "3C4D616B657246696C6520")
          , (0, "56657273696F6E20")
          ]
        , "application/vnd.mif"
        )
    )
  , ("xul",
        ( [ (0, "3C3F786D6C2076657273696F6E3D22312E30223F3E") ]
        , "application/vnd.mozilla.xul+xml"
        )
    )
  , ("asf",
        ( [ (0, "3026B2758E66CF11A6D900AA0062CE6C") ]
        , "application/vnd.ms-asf"
        )
    )
  , ("cab",
        ( [ (0, "49536328")
          , (0, "4D534346")
          ]
        , "application/vnd.ms-cab-compressed"
        )
    )
  , ("xls",
        ( [ (512, "0908100000060500")
          , (0, "D0CF11E0A1B11AE1")
          , (512, "FDFFFFFF04")
          , (512, "FDFFFFFF20000000")
          ]
        , "application/vnd.ms-excel"
        )
    )
  , ("xla",
        ( [ (0, "D0CF11E0A1B11AE1") ]
        , "application/vnd.ms-excel"
        )
    )
  , ("chm",
        ( [ (0, "49545346") ]
        , "application/vnd.ms-htmlhelp"
        )
    )
  , ("ppt",
        ( [ (512, "006E1EF0")
          , (512, "0F00E803")
          , (512, "A0461DF0")
          , (0, "D0CF11E0A1B11AE1")
          , (512, "FDFFFFFF04")
          ]
        , "application/vnd.ms-powerpoint"
        )
    )
  , ("pps",
        ( [ (0, "D0CF11E0A1B11AE1") ]
        , "application/vnd.ms-powerpoint"
        )
    )
  , ("wks",
        ( [ (0, "0E574B53")
          , (0, "FF000200040405540200")
          ]
        , "application/vnd.ms-works"
        )
    )
  , ("wpl",
        ( [ (84, "4D6963726F736F66742057696E646F7773204D6564696120506C61796572202D2D20") ]
        , "application/vnd.ms-wpl"
        )
    )
  , ("xps",
        ( [ (0, "504B0304") ]
        , "application/vnd.ms-xpsdocument"
        )
    )
  , ("cif",
        ( [ (2, "5B56657273696F6E") ]
        , "application/vnd.multiad.creator.cif"
        )
    )
  , ("odp",
        ( [ (0, "504B0304") ]
        , "application/vnd.oasis.opendocument.presentation"
        )
    )
  , ("odt",
        ( [ (0, "504B0304") ]
        , "application/vnd.oasis.opendocument.text"
        )
    )
  , ("ott",
        ( [ (0, "504B0304") ]
        , "application/vnd.oasis.opendocument.text-template"
        )
    )
  , ("pptx",
        ( [ (0, "504B030414000600") ]
        , "application/vnd.openxmlformats-officedocument.presentationml.presentation"
        )
    )
  , ("xlsx",
        ( [ (0, "504B030414000600") ]
        , "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
    )
  , ("docx",
        ( [ (0, "504B030414000600") ]
        , "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
    )
  , ("prc",
        ( [ (0, "424F4F4B4D4F4249")
          , (60, "74424D504B6E5772")
          ]
        , "application/vnd.palm"
        )
    )
  , ("pdb",
        ( [ (11, "000000000000000000000000000000000000000000000000")
          , (0, "4D2D5720506F636B6574204469637469")
          , (0, "4D6963726F736F667420432F432B2B20")
          , (0, "736D5F")
          , (0, "737A657A")
          , (0, "ACED0005737200126267626C69747A2E")
          ]
        , "application/vnd.palm"
        )
    )
  , ("qxd",
        ( [ (0, "00004D4D585052") ]
        , "application/vnd.Quark.QuarkXPress"
        )
    )
  , ("rar",
        ( [ (0, "526172211A0700")
          , (0, "526172211A070100")
          ]
        , "application/vnd.rar"
        )
    )
  , ("mmf",
        ( [ (0, "4D4D4D440000") ]
        , "application/vnd.smaf"
        )
    )
  , ("cap",
        ( [ (0, "52545353")
          , (0, "58435000")
          ]
        , "application/vnd.tcpdump.pcap"
        )
    )
  , ("dmp",
        ( [ (0, "4D444D5093A7")
          , (0, "5041474544553634")
          , (0, "5041474544554D50")
          ]
        , "application/vnd.tcpdump.pcap"
        )
    )
  , ("wpd",
        ( [ (0, "FF575043") ]
        , "application/vnd.wordperfect"
        )
    )
  , ("xar",
        ( [ (0, "78617221") ]
        , "application/vnd.xara"
        )
    )
  , ("spf",
        ( [ (0, "5350464900") ]
        , "application/vnd.yamaha.smaf-phrase"
        )
    )
  , ("dtd",
        ( [ (0, "0764743264647464") ]
        , "application/xml-dtd"
        )
    )
  , ("zip",
        ( [ (0, "504B0304")
          -- Will already match because '504B0304' matches
          -- , (0, "504B030414000100630000000000")
          , (0, "504B0708")
          , (30, "504B4C495445")
          , (526, "504B537058")
          , (29152, "57696E5A6970")
          ]
        , "application/zip"
        )
    )
  , ("amr",
        ( [ (0, "2321414D52") ]
        , "audio/AMR"
        )
    )
  , ("au",
        ( [ (0, "2E736E64")
          , (0, "646E732E")
          ]
        , "audio/basic"
        )
    )
  , ("m4a",
        ( [ (0, "00000020667479704D344120")
          , (4, "667479704D344120")
          ]
        , "audio/mp4"
        )
    )
  , ("mp3",
        ( [ (0, "494433")
          , (0, "FFFB")
          ]
        , "audio/mpeg"
        )
    )
  , ("oga",
        ( [ (0, "4F67675300020000000000000000") ]
        , "audio/ogg"
        )
    )
  , ("ogg",
        ( [ (0, "4F67675300020000000000000000") ]
        , "audio/ogg"
        )
    )
  , ("qcp",
        ( [ (0, "52494646") ]
        , "audio/qcelp"
        )
    )
  , ("koz",
        ( [ (0, "49443303000000") ]
        , "audio/vnd.audikoz"
        )
    )
  , ("bmp",
        ( [ (0, "424D") ]
        , "image/bmp"
        )
    )
  , ("dib",
        ( [ (0, "424D") ]
        , "image/bmp"
        )
    )
  , ("emf",
        ( [ (0, "01000000") ]
        , "image/emf"
        )
    )
  , ("fits",
        ( [ (0, "53494D504C4520203D202020202020202020202020202020202020202054") ]
        , "image/fits"
        )
    )
  , ("gif",
        ( [ (0, "474946383961") ]
        , "image/gif"
        )
    )
  , ("jp2",
        ( [ (0, "0000000C6A5020200D0A") ]
        , "image/jp2"
        )
    )
  , ("jpg",
        ( [ (0, "FFD8") ]
        , "image/jpeg"
        )
    )
  , ("jpeg",
        ( [ (0, "FFD8") ]
        , "image/jpeg"
        )
    )
  , ("jpe",
        ( [ (0, "FFD8") ]
        , "image/jpeg"
        )
    )
  , ("jfif",
        ( [ (0, "FFD8") ]
        , "image/jpeg"
        )
    )
  , ("png",
        ( [ (0, "89504E470D0A1A0A") ]
        , "image/png"
        )
    )
  , ("tiff",
        ( [ (0, "492049")
          , (0, "49492A00")
          , (0, "4D4D002A")
          , (0, "4D4D002B")
          ]
        , "image/tiff"
        )
    )
  , ("tif",
        ( [ (0, "492049")
          , (0, "49492A00")
          , (0, "4D4D002A")
          , (0, "4D4D002B")
          ]
        , "image/tiff"
        )
    )
  , ("psd",
        ( [ (0, "38425053") ]
        , "image/vnd.adobe.photoshop"
        )
    )
  , ("dwg",
        ( [ (0, "41433130") ]
        , "image/vnd.dwg"
        )
    )
  , ("ico",
        ( [ (0, "00000100") ]
        , "image/vnd.microsoft.icon"
        )
    )
  , ("mdi",
        ( [ (0, "4550") ]
        , "image/vnd.ms-modi"
        )
    )
  , ("hdr",
        ( [ (0, "233F52414449414E43450A")
          , (0, "49536328")
          ]
        , "image/vnd.radiance"
        )
    )
  , ("pcx",
        ( [ (512, "0908100000060500") ]
        , "image/vnd.zbrush.pcx"
        )
    )
  , ("wmf",
        ( [ (0, "010009000003")
          , (0, "D7CDC69A")
          ]
        , "image/wmf"
        )
    )
  , ("eml",
        ( [ (0, "46726F6D3A20")
          , (0, "52657475726E2D506174683A20")
          , (0, "582D")
          ]
        , "message/rfc822"
        )
    )
  , ("art",
        ( [ (0, "4A47040E") ]
        , "message/rfc822"
        )
    )
  , ("manifest",
        ( [ (0, "3C3F786D6C2076657273696F6E3D") ]
        , "text/cache-manifest"
        )
    )
  , ("log",
        ( [ (0, "2A2A2A2020496E7374616C6C6174696F6E205374617274656420") ]
        , "text/plain"
        )
    )
  , ("tsv",
        ( [ (0, "47") ]
        , "text/tab-separated-values"
        )
    )
  , ("vcf",
        ( [ (0, "424547494E3A56434152440D0A") ]
        , "text/vcard"
        )
    )
  , ("dms",
        ( [ (0, "444D5321") ]
        , "text/vnd.DMClientScript"
        )
    )
  , ("dot",
        ( [ (0, "D0CF11E0A1B11AE1") ]
        , "text/vnd.graphviz"
        )
    )
  , ("ts",
        ( [ (0, "47") ]
        , "text/vnd.trolltech.linguist"
        )
    )
  , ("3gp",
        ( [ (0, "0000001466747970336770")
          , (0, "0000002066747970336770")
          ]
        , "video/3gpp"
        )
    )
  , ("3g2",
        ( [ (0, "0000001466747970336770")
          , (0, "0000002066747970336770")
          ]
        , "video/3gpp2"
        )
    )
  , ("mp4",
        ( [ (0, "000000146674797069736F6D")
          , (0, "000000186674797033677035")
          , (0, "0000001C667479704D534E56012900464D534E566D703432")
          , (4, "6674797033677035")
          , (4, "667479704D534E56")
          , (4, "6674797069736F6D")
          ]
        , "video/mp4"
        )
    )
  , ("m4v",
        ( [ (0, "00000018667479706D703432")
          , (0, "00000020667479704D345620")
          , (4, "667479706D703432")
          ]
        , "video/mp4"
        )
    )
  , ("mpeg",
        ( [ (0, "00000100")
          , (0, "FFD8")
          ]
        , "video/mpeg"
        )
    )
  , ("mpg",
        ( [ (0, "00000100")
          , (0, "000001BA")
          , (0, "FFD8")
          ]
        , "video/mpeg"
        )
    )
  , ("ogv",
        ( [ (0, "4F67675300020000000000000000") ]
        , "video/ogg"
        )
    )
  , ("mov",
        ( [ (0, "00") -- I get the feeling this would match WAY too many files... any file that starts with 0?
          , (0, "000000146674797071742020")
          , (4, "6674797071742020")
          , (4, "6D6F6F76")
          ]
        , "video/quicktime"
        )
    )
  , ("cpt",
        ( [ (0, "4350543746494C45")
          , (0, "43505446494C45")
          ]
        , "application/mac-compactpro"
        )
    )
  , ("sxc",
        ( [ (0, "504B0304") ]
        , "application/vnd.sun.xml.calc"
        )
    )
  , ("sxd",
        ( [ (0, "504B0304") ]
        , "application/vnd.sun.xml.draw"
        )
    )
  , ("sxi",
        ( [ (0, "504B0304") ]
        , "application/vnd.sun.xml.impress"
        )
    )
  , ("sxw",
        ( [ (0, "504B0304") ]
        , "application/vnd.sun.xml.writer"
        )
    )
  , ("bz2",
        ( [ (0, "425A68") ]
        , "application/x-bzip2"
        )
    )
  , ("vcd",
        ( [ (0, "454E5452595643440200000102001858") ]
        , "application/x-cdlink"
        )
    )
  , ("csh",
        ( [ (0, "6375736800000002000000") ]
        , "application/x-csh"
        )
    )
  , ("spl",
        ( [ (0, "00000100") ]
        , "application/x-futuresplash"
        )
    )
  , ("jar",
        ( [ (0, "4A4152435300")
          , (0, "504B0304")
          -- Unneccessary, since '504B0304' would match it too
          -- , (0, "504B0304140008000800")
          , (0, "5F27A889")
          ]
        , "application/x-java-archive"
        )
    )
  , ("rpm",
        ( [ (0, "EDABEEDB") ]
        , "application/x-rpm"
        )
    )
  , ("swf",
        ( [ (0, "435753")
          , (0, "465753")
          , (0, "5A5753")
          ]
        , "application/x-shockwave-flash"
        )
    )
  , ("sit",
        ( [ (0, "5349542100")
          , (0, "5374756666497420286329313939372D")
          ]
        , "application/x-stuffit"
        )
    )
  , ("tar",
        ( [ (257, "7573746172") ]
        , "application/x-tar"
        )
    )
  , ("xpi",
        ( [ (0, "504B0304") ]
        , "application/x-xpinstall"
        )
    )
  , ("xz",
        ( [ (0, "FD377A585A00") ]
        , "application/x-xz"
        )
    )
  , ("mid",
        ( [ (0, "4D546864") ]
        , "audio/midi"
        )
    )
  , ("midi",
        ( [ (0, "4D546864") ]
        , "audio/midi"
        )
    )
  , ("aiff",
        ( [ (0, "464F524D00") ]
        , "audio/x-aiff"
        )
    )
  , ("flac",
        ( [ (0, "664C614300000022") ]
        , "audio/x-flac"
        )
    )
  , ("wma",
        ( [ (0, "3026B2758E66CF11A6D900AA0062CE6C") ]
        , "audio/x-ms-wma"
        )
    )
  , ("ram",
        ( [ (0, "727473703A2F2F") ]
        , "audio/x-pn-realaudio"
        )
    )
  , ("rm",
        ( [ (0, "2E524D46") ]
        , "audio/x-pn-realaudio"
        )
    )
  , ("ra",
        ( [ (0, "2E524D460000001200")
          , (0, "2E7261FD00")
          ]
        , "audio/x-realaudio"
        )
    )
  , ("wav",
        ( [ (0, "52494646") ]
        , "audio/x-wav"
        )
    )
  , ("webp",
        ( [ (0, "52494646") ]
        , "image/webp"
        )
    )
  , ("pgm",
        ( [ (0, "50350A") ]
        , "image/x-portable-graymap"
        )
    )
  , ("rgb",
        ( [ (0, "01DA01010003") ]
        , "image/x-rgb"
        )
    )
  , ("webm",
        ( [ (0, "1A45DFA3") ]
        , "video/webm"
        )
    )
  , ("flv",
        ( [ (0, "00000020667479704D345620")
          , (0, "464C5601")
          ]
        , "video/x-flv"
        )
    )
  , ("mkv",
        ( [ (0, "1A45DFA3") ]
        , "video/x-matroska"
        )
    )
  , ("asx",
        ( [ (0, "3C") ]
        , "video/x-ms-asf"
        )
    )
  , ("wmv",
        ( [ (0, "3026B2758E66CF11A6D900AA0062CE6C") ]
        , "video/x-ms-wmv"
        )
    )
  , ("avi",
        ( [ (0, "52494646") ]
        , "video/x-msvideo"
        )
    )
  ]
