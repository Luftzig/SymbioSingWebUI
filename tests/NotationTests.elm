module NotationTests exposing (..)

import Dict
import Expect
import Notation exposing (Timing(..), parseMusicXml)
import Test exposing (..)


parseMusicXmlSuite : Test
parseMusicXmlSuite =
    let
        parsedContent =
            parseMusicXml testContent
    in
    describe "test the parsing of a musicXML file"
        [ test "parsing succeeds" <|
            \_ ->
                parsedContent
                    |> Expect.ok
        , test "parsing extracts parts with correct ids" <|
            \_ ->
                case parsedContent of
                    Err e ->
                        Expect.fail ("parsing failed with error \"" ++ e ++ "\"")

                    Ok parts ->
                        Dict.keys parts |> Expect.equalLists [ "P1", "P2" ]
        , test "parsing extracts partss with correct names" <|
            \_ ->
                case parsedContent of
                    Err e ->
                        Expect.fail ("parsing failed with error \"" ++ e ++ "\"")

                    Ok parts ->
                        Dict.values parts
                            |> List.map .name
                            |> Expect.equalLists [ "Timpani", "Timpani" ]
        , test "read time signature for first measure" <|
            \_ ->
                case parsedContent of
                    Err e ->
                        Expect.fail ("parsing failed with error \"" ++ e ++ "\"")

                    Ok parts ->
                        parts
                            |> Dict.get "P1"
                            |> Maybe.map .measures
                            |> Maybe.andThen List.head
                            |> Maybe.map .signature
                            |> Expect.equal (Just { beats = 4, beatType = Duration 4 })
        ]


testContent =
    """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 3.1 Partwise//EN" "http://www.musicxml.org/dtds/partwise.dtd"/>
<score-partwise version="3.1">
  <work>
    <work-title>Title</work-title>
    </work>
  <identification>
    <creator type="composer">Composer</creator>
    <encoding>
      <software>MuseScore 3.6.2</software>
      <encoding-date>2022-01-03</encoding-date>
      <supports element="accidental" type="yes"/>
      <supports element="beam" type="yes"/>
      <supports element="print" attribute="new-page" type="yes" value="yes"/>
      <supports element="print" attribute="new-system" type="yes" value="yes"/>
      <supports element="stem" type="yes"/>
      </encoding>
    </identification>
  <defaults>
    <scaling>
      <millimeters>6.99911</millimeters>
      <tenths>40</tenths>
      </scaling>
    <page-layout>
      <page-height>1596.77</page-height>
      <page-width>1233.87</page-width>
      <page-margins type="even">
        <left-margin>85.7252</left-margin>
        <right-margin>85.7252</right-margin>
        <top-margin>85.7252</top-margin>
        <bottom-margin>85.7252</bottom-margin>
        </page-margins>
      <page-margins type="odd">
        <left-margin>85.7252</left-margin>
        <right-margin>85.7252</right-margin>
        <top-margin>85.7252</top-margin>
        <bottom-margin>85.7252</bottom-margin>
        </page-margins>
      </page-layout>
    <word-font font-family="Edwin" font-size="10"/>
    <lyric-font font-family="Edwin" font-size="10"/>
    </defaults>
  <credit page="1">
    <credit-type>title</credit-type>
    <credit-words default-x="616.935" default-y="1511.04" justify="center" valign="top" font-size="22">Title</credit-words>
    </credit>
  <credit page="1">
    <credit-type>composer</credit-type>
    <credit-words default-x="1148.14" default-y="1411.04" justify="right" valign="bottom">Composer</credit-words>
    </credit>
  <part-list>
    <score-part id="P1">
      <part-name>Timpani</part-name>
      <part-abbreviation>Timp.</part-abbreviation>
      <score-instrument id="P1-I1">
        <instrument-name>Timpani</instrument-name>
        </score-instrument>
      <midi-device id="P1-I1" port="1"></midi-device>
      <midi-instrument id="P1-I1">
        <midi-channel>1</midi-channel>
        <midi-program>48</midi-program>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P2">
      <part-name>Timpani</part-name>
      <part-abbreviation>Timp.</part-abbreviation>
      <score-instrument id="P2-I1">
        <instrument-name>Timpani</instrument-name>
        </score-instrument>
      <midi-device id="P2-I1" port="1"></midi-device>
      <midi-instrument id="P2-I1">
        <midi-channel>2</midi-channel>
        <midi-program>48</midi-program>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    </part-list>
  <part id="P1">
    <measure number="1" width="594.45">
      <print>
        <system-layout>
          <system-margins>
            <left-margin>88.25</left-margin>
            <right-margin>-0.00</right-margin>
            </system-margins>
          <top-system-distance>170.00</top-system-distance>
          </system-layout>
        </print>
      <attributes>
        <divisions>4</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
          </time>
        <clef>
          <sign>F</sign>
          <line>4</line>
          </clef>
        </attributes>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>half</type>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note>
        <rest/>
        <duration>1</duration>
        <voice>1</voice>
        <type>16th</type>
        </note>
      <note>
        <rest/>
        <duration>1</duration>
        <voice>1</voice>
        <type>16th</type>
        </note>
      <note>
        <rest/>
        <duration>1</duration>
        <voice>1</voice>
        <type>16th</type>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note default-x="411.79" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="471.63" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <accidental>flat</accidental>
        <stem>up</stem>
        <notations>
          <slur type="start" placement="below" number="1"/>
          </notations>
        </note>
      <note default-x="520.68" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        <notations>
          <slur type="stop" number="1"/>
          </notations>
        </note>
      </measure>
    <measure number="2" width="119.43">
      <note default-x="19.32" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <accidental>flat</accidental>
        <stem>up</stem>
        <notations>
          <slur type="start" placement="below" number="1"/>
          </notations>
        </note>
      <note default-x="42.76" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        <notations>
          <slur type="stop" number="1"/>
          </notations>
        </note>
      <note default-x="66.20" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        <notations>
          <slur type="start" placement="below" number="1"/>
          </notations>
        </note>
      <note default-x="89.63" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        <notations>
          <slur type="stop" number="1"/>
          </notations>
        </note>
      </measure>
    <measure number="3" width="119.43">
      <note default-x="19.32" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <accidental>flat</accidental>
        <stem>up</stem>
        </note>
      <note default-x="42.76" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="66.20" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="89.63" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      </measure>
    <measure number="4" width="140.85">
      <note default-x="19.32" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <accidental>flat</accidental>
        <stem>up</stem>
        </note>
      <note default-x="42.76" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="66.20" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="89.63" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      </measure>
    <measure number="5" width="157.99">
      <print new-system="yes">
        <system-layout>
          <system-margins>
            <left-margin>62.91</left-margin>
            <right-margin>-0.00</right-margin>
            </system-margins>
          <system-distance>150.00</system-distance>
          </system-layout>
        </print>
      <attributes>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        </attributes>
      <note default-x="81.47" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="116.63" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>2</duration>
        <tie type="start"/>
        <voice>1</voice>
        <type>eighth</type>
        <accidental>flat</accidental>
        <stem>up</stem>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      </measure>
    <measure number="6" width="75.74">
      <note default-x="13.00" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>2</duration>
        <tie type="stop"/>
        <voice>1</voice>
        <type>eighth</type>
        <stem>up</stem>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note default-x="39.56" default-y="-40.00">
        <pitch>
          <step>G</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      </measure>
    <measure number="7" width="75.74">
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note default-x="34.38" default-y="-30.00">
        <pitch>
          <step>B</step>
          <octave>2</octave>
          </pitch>
        <duration>2</duration>
        <tie type="start"/>
        <voice>1</voice>
        <type>eighth</type>
        <stem>up</stem>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      </measure>
    <measure number="8" width="90.68">
      <note default-x="13.00" default-y="-30.00">
        <pitch>
          <step>B</step>
          <octave>2</octave>
          </pitch>
        <duration>2</duration>
        <tie type="stop"/>
        <voice>1</voice>
        <type>eighth</type>
        <stem>up</stem>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="9" width="141.65">
      <attributes>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
          </time>
        </attributes>
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      <backup>
        <duration>16</duration>
        </backup>
      <note default-x="42.54" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>6</duration>
        <voice>2</voice>
        <type>quarter</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note default-x="77.19" default-y="-45.00">
        <pitch>
          <step>F</step>
          <octave>2</octave>
          </pitch>
        <duration>6</duration>
        <voice>2</voice>
        <type>quarter</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>2</voice>
        <type>quarter</type>
        </note>
      </measure>
    <measure number="10" width="147.45">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      <backup>
        <duration>16</duration>
        </backup>
      <note default-x="18.56" default-y="-25.00">
        <pitch>
          <step>C</step>
          <octave>3</octave>
          </pitch>
        <duration>2</duration>
        <voice>2</voice>
        <type>eighth</type>
        <stem>down</stem>
        <beam number="1">begin</beam>
        </note>
      <note default-x="37.65" default-y="-30.00">
        <pitch>
          <step>B</step>
          <octave>2</octave>
          </pitch>
        <duration>2</duration>
        <voice>2</voice>
        <type>eighth</type>
        <stem>down</stem>
        <beam number="1">continue</beam>
        </note>
      <note default-x="65.47" default-y="-30.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>2</duration>
        <voice>2</voice>
        <type>eighth</type>
        <accidental>flat</accidental>
        <stem>down</stem>
        <beam number="1">continue</beam>
        </note>
      <note default-x="84.56" default-y="-35.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>2</duration>
        <voice>2</voice>
        <type>eighth</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>2</voice>
        <type>half</type>
        </note>
      </measure>
    <measure number="11" width="61.72">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="12" width="61.72">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="13" width="61.72">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="14" width="61.72">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="15" width="63.37">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P2">
    <measure number="1" width="594.45">
      <print>
        <staff-layout number="1">
          <staff-distance>65.00</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>4</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
          </time>
        <clef>
          <sign>F</sign>
          <line>4</line>
          </clef>
        </attributes>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note default-x="120.04" default-y="-140.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>8</duration>
        <voice>1</voice>
        <type>half</type>
        <stem>up</stem>
        </note>
      <note default-x="166.59" default-y="-150.00">
        <pitch>
          <step>F</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="203.16" default-y="-150.00">
        <pitch>
          <step>F</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="239.73" default-y="-140.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>1</duration>
        <voice>1</voice>
        <type>16th</type>
        </note>
      </measure>
    <measure number="2" width="119.43">
      <note default-x="19.32" default-y="-135.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <accidental>flat</accidental>
        <stem>up</stem>
        </note>
      <note default-x="42.76" default-y="-140.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="66.20" default-y="-135.00">
        <pitch>
          <step>B</step>
          <alter>-1</alter>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      <note default-x="89.63" default-y="-140.00">
        <pitch>
          <step>A</step>
          <octave>2</octave>
          </pitch>
        <duration>4</duration>
        <voice>1</voice>
        <type>quarter</type>
        <stem>up</stem>
        </note>
      </measure>
    <measure number="3" width="119.43">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="4" width="140.85">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="5" width="157.99">
      <print new-system="yes">
        <staff-layout number="1">
          <staff-distance>65.00</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        </attributes>
      <note>
        <rest measure="yes"/>
        <duration>6</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="6" width="75.74">
      <note>
        <rest measure="yes"/>
        <duration>6</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="7" width="75.74">
      <note>
        <rest measure="yes"/>
        <duration>6</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="8" width="90.68">
      <note>
        <rest measure="yes"/>
        <duration>6</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="9" width="141.65">
      <attributes>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
          </time>
        </attributes>
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="10" width="147.45">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="11" width="61.72">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="12" width="61.72">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="13" width="61.72">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="14" width="61.72">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      </measure>
    <measure number="15" width="63.37">
      <note>
        <rest measure="yes"/>
        <duration>16</duration>
        <voice>1</voice>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  </score-partwise>
    """
