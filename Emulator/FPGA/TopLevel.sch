<?xml version="1.0" encoding="UTF-8"?>
<drawing version="7">
    <attr value="spartan3e" name="DeviceFamilyName">
        <trait delete="all:0" />
        <trait editname="all:0" />
        <trait edittrait="all:0" />
    </attr>
    <netlist>
        <signal name="seg(6:0)" />
        <signal name="an(3:0)" />
        <signal name="BTN(3:0)" />
        <signal name="SW(7:0)" />
        <signal name="CLK1" />
        <signal name="LED(7:0)" />
        <signal name="ADDR(7:0)" />
        <signal name="ADDR(11:0)" />
        <signal name="XLXN_78" />
        <signal name="XLXN_82(11:0)" />
        <signal name="ControlBus(11:0)" />
        <signal name="ControlBus(1)" />
        <signal name="ControlBus(0)" />
        <signal name="XLXN_93(3:0)" />
        <signal name="XLXN_95(11:0)" />
        <signal name="XLXN_97(11:0)" />
        <signal name="XLXN_99(11:0)" />
        <signal name="XLXN_101(11:0)" />
        <signal name="XLXN_102" />
        <signal name="XLXN_103" />
        <signal name="XLXN_104" />
        <port polarity="Output" name="seg(6:0)" />
        <port polarity="Output" name="an(3:0)" />
        <port polarity="Input" name="BTN(3:0)" />
        <port polarity="Input" name="SW(7:0)" />
        <port polarity="Input" name="CLK1" />
        <port polarity="Output" name="LED(7:0)" />
        <blockdef name="obuf8">
            <timestamp>2000-1-1T10:10:10</timestamp>
            <line x2="64" y1="-32" y2="-32" x1="0" />
            <line x2="64" y1="0" y2="-64" x1="64" />
            <line x2="64" y1="-32" y2="0" x1="128" />
            <line x2="128" y1="-64" y2="-32" x1="64" />
            <line x2="128" y1="-32" y2="-32" x1="224" />
            <rect width="64" x="0" y="-44" height="24" />
            <rect width="96" x="128" y="-44" height="24" />
        </blockdef>
        <blockdef name="Seg7Driver">
            <timestamp>2011-9-25T23:54:25</timestamp>
            <rect width="64" x="320" y="20" height="24" />
            <line x2="384" y1="32" y2="32" x1="320" />
            <line x2="0" y1="-96" y2="-96" x1="64" />
            <rect width="64" x="0" y="-44" height="24" />
            <line x2="0" y1="-32" y2="-32" x1="64" />
            <rect width="64" x="320" y="-108" height="24" />
            <line x2="384" y1="-96" y2="-96" x1="320" />
            <rect width="64" x="320" y="-44" height="24" />
            <line x2="384" y1="-32" y2="-32" x1="320" />
            <rect width="256" x="64" y="-128" height="192" />
        </blockdef>
        <blockdef name="InputController">
            <timestamp>2011-9-27T4:49:44</timestamp>
            <line x2="384" y1="160" y2="160" x1="320" />
            <rect width="64" x="320" y="84" height="24" />
            <line x2="384" y1="96" y2="96" x1="320" />
            <rect width="64" x="0" y="20" height="24" />
            <line x2="0" y1="32" y2="32" x1="64" />
            <line x2="0" y1="-288" y2="-288" x1="64" />
            <rect width="64" x="0" y="-172" height="24" />
            <line x2="0" y1="-160" y2="-160" x1="64" />
            <line x2="384" y1="-288" y2="-288" x1="320" />
            <line x2="384" y1="-224" y2="-224" x1="320" />
            <line x2="384" y1="-160" y2="-160" x1="320" />
            <rect width="64" x="320" y="-108" height="24" />
            <line x2="384" y1="-96" y2="-96" x1="320" />
            <rect width="64" x="320" y="-44" height="24" />
            <line x2="384" y1="-32" y2="-32" x1="320" />
            <rect width="256" x="64" y="-320" height="512" />
        </blockdef>
        <blockdef name="RAM">
            <timestamp>2011-9-27T0:55:23</timestamp>
            <rect width="64" x="0" y="148" height="24" />
            <line x2="0" y1="160" y2="160" x1="64" />
            <rect width="64" x="320" y="84" height="24" />
            <line x2="384" y1="96" y2="96" x1="320" />
            <line x2="0" y1="-224" y2="-224" x1="64" />
            <rect width="64" x="0" y="-44" height="24" />
            <line x2="0" y1="-32" y2="-32" x1="64" />
            <rect width="256" x="64" y="-256" height="448" />
        </blockdef>
        <blockdef name="BussedALU">
            <timestamp>2011-9-27T0:57:20</timestamp>
            <rect width="304" x="64" y="-128" height="128" />
            <rect width="64" x="0" y="-108" height="24" />
            <line x2="0" y1="-96" y2="-96" x1="64" />
            <rect width="64" x="368" y="-108" height="24" />
            <line x2="432" y1="-96" y2="-96" x1="368" />
        </blockdef>
        <blockdef name="MicrocodeProcessor">
            <timestamp>2011-9-27T4:49:53</timestamp>
            <line x2="0" y1="352" y2="352" x1="64" />
            <rect width="64" x="320" y="212" height="24" />
            <line x2="384" y1="224" y2="224" x1="320" />
            <rect width="64" x="320" y="84" height="24" />
            <line x2="384" y1="96" y2="96" x1="320" />
            <line x2="0" y1="-608" y2="-608" x1="64" />
            <rect width="64" x="320" y="-172" height="24" />
            <line x2="384" y1="-160" y2="-160" x1="320" />
            <rect width="64" x="320" y="-44" height="24" />
            <line x2="384" y1="-32" y2="-32" x1="320" />
            <rect width="256" x="64" y="-640" height="1024" />
        </blockdef>
        <block symbolname="Seg7Driver" name="SegDriver">
            <blockpin signalname="CLK1" name="clk" />
            <blockpin signalname="XLXN_82(11:0)" name="data(11:0)" />
            <blockpin signalname="XLXN_93(3:0)" name="nibble(3:0)" />
            <blockpin signalname="seg(6:0)" name="seg(6:0)" />
            <blockpin signalname="an(3:0)" name="an(3:0)" />
        </block>
        <block symbolname="InputController" name="IO_Manager">
            <blockpin signalname="CLK1" name="clk" />
            <blockpin signalname="SW(7:0)" name="sw(7:0)" />
            <blockpin signalname="BTN(3:0)" name="RAWbtn(3:0)" />
            <blockpin signalname="ControlBus(1)" name="RAMwrite" />
            <blockpin signalname="ControlBus(0)" name="RAMread" />
            <blockpin signalname="XLXN_78" name="MicroCodeCLK" />
            <blockpin signalname="XLXN_82(11:0)" name="Data(11:0)" />
            <blockpin signalname="ADDR(11:0)" name="Addr(11:0)" />
            <blockpin name="outNibble(3:0)" />
            <blockpin signalname="XLXN_104" name="MicroCodeEN" />
        </block>
        <block symbolname="obuf8" name="XLXI_11">
            <blockpin signalname="ADDR(7:0)" name="I(7:0)" />
            <blockpin signalname="LED(7:0)" name="O(7:0)" />
        </block>
        <block symbolname="RAM" name="SystemRam">
            <blockpin signalname="CLK1" name="CLK" />
            <blockpin signalname="ADDR(11:0)" name="ADDR(11:0)" />
            <blockpin signalname="XLXN_82(11:0)" name="IO(11:0)" />
            <blockpin signalname="ControlBus(11:0)" name="Control(11:0)" />
        </block>
        <block symbolname="BussedALU" name="XLXI_12">
            <blockpin signalname="ControlBus(11:0)" name="CBus(11:0)" />
            <blockpin signalname="XLXN_82(11:0)" name="DataBus(11:0)" />
        </block>
        <block symbolname="MicrocodeProcessor" name="XLXI_14">
            <blockpin signalname="XLXN_78" name="CLK" />
            <blockpin signalname="XLXN_82(11:0)" name="DataBus(11:0)" />
            <blockpin signalname="XLXN_93(3:0)" name="microcode(3:0)" />
            <blockpin signalname="ADDR(11:0)" name="AddrBus(11:0)" />
            <blockpin signalname="ControlBus(11:0)" name="Control(11:0)" />
            <blockpin signalname="XLXN_104" name="EN" />
        </block>
    </netlist>
    <sheet sheetnum="1" width="3520" height="2720">
        <iomarker fontsize="28" x="704" y="976" name="seg(6:0)" orien="R180" />
        <iomarker fontsize="28" x="736" y="1392" name="an(3:0)" orien="R180" />
        <instance x="64" y="1296" name="SegDriver" orien="R0">
            <attrtext style="fontsize:28;fontname:Arial" attrname="InstName" x="160" y="-80" type="instance" />
        </instance>
        <branch name="seg(6:0)">
            <wire x2="1040" y1="1200" y2="1200" x1="448" />
            <wire x2="1040" y1="976" y2="976" x1="704" />
            <wire x2="1040" y1="976" y2="1200" x1="1040" />
        </branch>
        <branch name="an(3:0)">
            <wire x2="1040" y1="1264" y2="1264" x1="448" />
            <wire x2="1040" y1="1264" y2="1392" x1="1040" />
            <wire x2="1040" y1="1392" y2="1392" x1="736" />
        </branch>
        <iomarker fontsize="28" x="304" y="480" name="BTN(3:0)" orien="R180" />
        <branch name="SW(7:0)">
            <wire x2="528" y1="160" y2="160" x1="320" />
            <wire x2="528" y1="160" y2="416" x1="528" />
            <wire x2="848" y1="416" y2="416" x1="528" />
        </branch>
        <iomarker fontsize="28" x="320" y="160" name="SW(7:0)" orien="R180" />
        <instance x="848" y="576" name="IO_Manager" orien="R0">
            <attrtext style="fontsize:28;fontname:Arial" attrname="InstName" x="128" y="16" type="instance" />
        </instance>
        <iomarker fontsize="28" x="96" y="704" name="CLK1" orien="R0" />
        <branch name="BTN(3:0)">
            <wire x2="528" y1="480" y2="480" x1="304" />
            <wire x2="528" y1="480" y2="544" x1="528" />
            <wire x2="688" y1="544" y2="544" x1="528" />
            <wire x2="688" y1="544" y2="608" x1="688" />
            <wire x2="848" y1="608" y2="608" x1="688" />
        </branch>
        <instance x="2592" y="1264" name="XLXI_11" orien="R0" />
        <branch name="LED(7:0)">
            <wire x2="3232" y1="1232" y2="1232" x1="2816" />
        </branch>
        <branch name="ADDR(7:0)">
            <wire x2="2448" y1="1120" y2="1120" x1="2304" />
            <wire x2="2448" y1="1120" y2="1232" x1="2448" />
            <wire x2="2592" y1="1232" y2="1232" x1="2448" />
        </branch>
        <bustap x2="2304" y1="1120" y2="1120" x1="2208" />
        <iomarker fontsize="28" x="3232" y="1232" name="LED(7:0)" orien="R0" />
        <branch name="CLK1">
            <wire x2="704" y1="64" y2="64" x1="48" />
            <wire x2="704" y1="64" y2="288" x1="704" />
            <wire x2="848" y1="288" y2="288" x1="704" />
            <wire x2="48" y1="64" y2="704" x1="48" />
            <wire x2="96" y1="704" y2="704" x1="48" />
            <wire x2="48" y1="704" y2="880" x1="48" />
            <wire x2="48" y1="880" y2="1200" x1="48" />
            <wire x2="64" y1="1200" y2="1200" x1="48" />
            <wire x2="1408" y1="880" y2="880" x1="48" />
            <wire x2="1408" y1="880" y2="896" x1="1408" />
            <wire x2="1408" y1="896" y2="896" x1="1392" />
            <wire x2="1392" y1="896" y2="1840" x1="1392" />
            <wire x2="1408" y1="1840" y2="1840" x1="1392" />
        </branch>
        <instance x="1424" y="2512" name="XLXI_12" orien="R0">
        </instance>
        <instance x="1408" y="2064" name="SystemRam" orien="R0">
            <attrtext style="fontsize:28;fontname:Arial" attrname="InstName" x="144" y="-160" type="instance" />
        </instance>
        <branch name="ADDR(11:0)">
            <wire x2="960" y1="2016" y2="2016" x1="688" />
            <wire x2="960" y1="1936" y2="2016" x1="960" />
            <wire x2="1248" y1="1936" y2="1936" x1="960" />
            <wire x2="1248" y1="1936" y2="2032" x1="1248" />
            <wire x2="1408" y1="2032" y2="2032" x1="1248" />
            <wire x2="1616" y1="544" y2="544" x1="1232" />
            <wire x2="1616" y1="544" y2="1120" x1="1616" />
            <wire x2="2208" y1="1120" y2="1120" x1="1616" />
            <wire x2="1536" y1="1776" y2="1776" x1="1248" />
            <wire x2="1248" y1="1776" y2="1936" x1="1248" />
            <wire x2="1616" y1="1120" y2="1120" x1="1536" />
            <wire x2="1536" y1="1120" y2="1776" x1="1536" />
        </branch>
        <branch name="ControlBus(11:0)">
            <wire x2="992" y1="2272" y2="2272" x1="688" />
            <wire x2="992" y1="2272" y2="2544" x1="992" />
            <wire x2="1312" y1="2544" y2="2544" x1="992" />
            <wire x2="1312" y1="2544" y2="2624" x1="1312" />
            <wire x2="2224" y1="2624" y2="2624" x1="1312" />
            <wire x2="1408" y1="2224" y2="2224" x1="1312" />
            <wire x2="1312" y1="2224" y2="2416" x1="1312" />
            <wire x2="1424" y1="2416" y2="2416" x1="1312" />
            <wire x2="1312" y1="2416" y2="2544" x1="1312" />
            <wire x2="2224" y1="176" y2="240" x1="2224" />
            <wire x2="2224" y1="240" y2="336" x1="2224" />
            <wire x2="2224" y1="336" y2="2624" x1="2224" />
        </branch>
        <bustap x2="2128" y1="240" y2="240" x1="2224" />
        <bustap x2="2128" y1="336" y2="336" x1="2224" />
        <branch name="ControlBus(1)">
            <wire x2="1680" y1="288" y2="288" x1="1232" />
            <wire x2="1680" y1="240" y2="288" x1="1680" />
            <wire x2="2128" y1="240" y2="240" x1="1680" />
        </branch>
        <branch name="ControlBus(0)">
            <wire x2="1680" y1="352" y2="352" x1="1232" />
            <wire x2="1680" y1="336" y2="352" x1="1680" />
            <wire x2="2128" y1="336" y2="336" x1="1680" />
        </branch>
        <branch name="XLXN_93(3:0)">
            <wire x2="848" y1="1328" y2="1328" x1="448" />
            <wire x2="848" y1="1328" y2="2400" x1="848" />
            <wire x2="848" y1="2400" y2="2400" x1="688" />
        </branch>
        <instance x="304" y="2176" name="XLXI_14" orien="R0">
        </instance>
        <branch name="XLXN_78">
            <wire x2="240" y1="1456" y2="1568" x1="240" />
            <wire x2="304" y1="1568" y2="1568" x1="240" />
            <wire x2="1248" y1="1456" y2="1456" x1="240" />
            <wire x2="1248" y1="416" y2="416" x1="1232" />
            <wire x2="1248" y1="416" y2="1456" x1="1248" />
        </branch>
        <branch name="XLXN_82(11:0)">
            <wire x2="64" y1="1264" y2="1264" x1="16" />
            <wire x2="16" y1="1264" y2="1440" x1="16" />
            <wire x2="1152" y1="1440" y2="1440" x1="16" />
            <wire x2="1152" y1="1440" y2="1680" x1="1152" />
            <wire x2="2032" y1="1680" y2="1680" x1="1152" />
            <wire x2="2032" y1="1680" y2="2272" x1="2032" />
            <wire x2="2032" y1="2272" y2="2336" x1="2032" />
            <wire x2="768" y1="2144" y2="2144" x1="688" />
            <wire x2="768" y1="2144" y2="2336" x1="768" />
            <wire x2="2032" y1="2336" y2="2336" x1="768" />
            <wire x2="2032" y1="480" y2="480" x1="1232" />
            <wire x2="2032" y1="480" y2="1680" x1="2032" />
            <wire x2="1872" y1="2160" y2="2160" x1="1792" />
            <wire x2="1872" y1="2160" y2="2272" x1="1872" />
            <wire x2="1872" y1="2272" y2="2416" x1="1872" />
            <wire x2="2032" y1="2272" y2="2272" x1="1872" />
            <wire x2="1872" y1="2416" y2="2416" x1="1856" />
        </branch>
        <branch name="XLXN_104">
            <wire x2="304" y1="2528" y2="2528" x1="256" />
            <wire x2="256" y1="2528" y2="2656" x1="256" />
            <wire x2="592" y1="2656" y2="2656" x1="256" />
            <wire x2="2576" y1="2656" y2="2656" x1="592" />
            <wire x2="3280" y1="2656" y2="2656" x1="2576" />
            <wire x2="2208" y1="736" y2="736" x1="1232" />
            <wire x2="2208" y1="592" y2="736" x1="2208" />
            <wire x2="3216" y1="592" y2="592" x1="2208" />
            <wire x2="3216" y1="592" y2="1856" x1="3216" />
            <wire x2="3280" y1="1856" y2="1856" x1="3216" />
            <wire x2="3280" y1="1856" y2="2656" x1="3280" />
        </branch>
    </sheet>
</drawing>