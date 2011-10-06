<?xml version="1.0" encoding="UTF-8"?>
<drawing version="7">
    <attr value="spartan3e" name="DeviceFamilyName">
        <trait delete="all:0" />
        <trait editname="all:0" />
        <trait edittrait="all:0" />
    </attr>
    <netlist>
        <signal name="DataBus(11:0)" />
        <signal name="XLXN_4(11:0)" />
        <signal name="XLXN_5(11:0)" />
        <signal name="CBus(11:0)" />
        <signal name="CBus(2)" />
        <signal name="CBus(3)" />
        <signal name="CBus(4)" />
        <signal name="CBus(5)" />
        <signal name="CBus(11:8)">
        </signal>
        <signal name="XLXN_8" />
        <signal name="XLXN_9" />
        <port polarity="BiDirectional" name="DataBus(11:0)" />
        <port polarity="Input" name="CBus(11:0)" />
        <blockdef name="ALU">
            <timestamp>2011-9-25T2:28:28</timestamp>
            <rect width="256" x="64" y="-320" height="320" />
            <line x2="0" y1="-288" y2="-288" x1="64" />
            <line x2="0" y1="-224" y2="-224" x1="64" />
            <rect width="64" x="0" y="-172" height="24" />
            <line x2="0" y1="-160" y2="-160" x1="64" />
            <rect width="64" x="0" y="-108" height="24" />
            <line x2="0" y1="-96" y2="-96" x1="64" />
            <rect width="64" x="0" y="-44" height="24" />
            <line x2="0" y1="-32" y2="-32" x1="64" />
            <rect width="64" x="320" y="-300" height="24" />
            <line x2="384" y1="-288" y2="-288" x1="320" />
        </blockdef>
        <blockdef name="WordRegister">
            <timestamp>2011-9-24T0:49:44</timestamp>
            <rect width="256" x="64" y="-128" height="128" />
            <line x2="0" y1="-96" y2="-96" x1="64" />
            <line x2="0" y1="-32" y2="-32" x1="64" />
            <rect width="64" x="320" y="-108" height="24" />
            <line x2="384" y1="-96" y2="-96" x1="320" />
            <rect width="64" x="320" y="-44" height="24" />
            <line x2="384" y1="-32" y2="-32" x1="320" />
        </blockdef>
        <blockdef name="gnd">
            <timestamp>2000-1-1T10:10:10</timestamp>
            <line x2="64" y1="-64" y2="-96" x1="64" />
            <line x2="52" y1="-48" y2="-48" x1="76" />
            <line x2="60" y1="-32" y2="-32" x1="68" />
            <line x2="40" y1="-64" y2="-64" x1="88" />
            <line x2="64" y1="-64" y2="-80" x1="64" />
            <line x2="64" y1="-128" y2="-96" x1="64" />
        </blockdef>
        <block symbolname="ALU" name="XLXI_1">
            <blockpin signalname="CBus(5)" name="En" />
            <blockpin signalname="CBus(4)" name="LM" />
            <blockpin signalname="XLXN_4(11:0)" name="A(11:0)" />
            <blockpin signalname="XLXN_5(11:0)" name="B(11:0)" />
            <blockpin signalname="CBus(11:8)" name="Func(3:0)" />
            <blockpin signalname="DataBus(11:0)" name="Output(11:0)" />
        </block>
        <block symbolname="WordRegister" name="A_Register">
            <blockpin signalname="CBus(2)" name="WR" />
            <blockpin signalname="XLXN_9" name="OEN" />
            <blockpin signalname="XLXN_4(11:0)" name="OUTPUT(11:0)" />
            <blockpin signalname="DataBus(11:0)" name="DATABUS(11:0)" />
        </block>
        <block symbolname="WordRegister" name="B_Register">
            <blockpin signalname="CBus(3)" name="WR" />
            <blockpin signalname="XLXN_9" name="OEN" />
            <blockpin signalname="XLXN_5(11:0)" name="OUTPUT(11:0)" />
            <blockpin signalname="DataBus(11:0)" name="DATABUS(11:0)" />
        </block>
        <block symbolname="gnd" name="XLXI_4">
            <blockpin signalname="XLXN_9" name="G" />
        </block>
    </netlist>
    <sheet sheetnum="1" width="3520" height="2720">
        <instance x="1792" y="1408" name="XLXI_1" orien="R0">
        </instance>
        <instance x="1104" y="1200" name="A_Register" orien="R0">
            <attrtext style="fontsize:28;fontname:Arial" attrname="InstName" x="128" y="-64" type="instance" />
        </instance>
        <instance x="1104" y="1440" name="B_Register" orien="R0">
            <attrtext style="fontsize:28;fontname:Arial" attrname="InstName" x="144" y="-64" type="instance" />
        </instance>
        <branch name="DataBus(11:0)">
            <wire x2="1568" y1="1168" y2="1168" x1="1488" />
            <wire x2="1568" y1="1168" y2="1472" x1="1568" />
            <wire x2="2256" y1="1472" y2="1472" x1="1568" />
            <wire x2="2400" y1="1472" y2="1472" x1="2256" />
            <wire x2="1520" y1="1408" y2="1408" x1="1488" />
            <wire x2="1520" y1="1408" y2="1472" x1="1520" />
            <wire x2="1568" y1="1472" y2="1472" x1="1520" />
            <wire x2="2256" y1="1120" y2="1120" x1="2176" />
            <wire x2="2256" y1="1120" y2="1472" x1="2256" />
        </branch>
        <branch name="XLXN_4(11:0)">
            <wire x2="1632" y1="1104" y2="1104" x1="1488" />
            <wire x2="1632" y1="1104" y2="1248" x1="1632" />
            <wire x2="1792" y1="1248" y2="1248" x1="1632" />
        </branch>
        <branch name="XLXN_5(11:0)">
            <wire x2="1632" y1="1344" y2="1344" x1="1488" />
            <wire x2="1632" y1="1312" y2="1344" x1="1632" />
            <wire x2="1792" y1="1312" y2="1312" x1="1632" />
        </branch>
        <iomarker fontsize="28" x="2400" y="1472" name="DataBus(11:0)" orien="R0" />
        <branch name="CBus(11:0)">
            <wire x2="800" y1="880" y2="880" x1="640" />
            <wire x2="800" y1="880" y2="928" x1="800" />
            <wire x2="800" y1="928" y2="976" x1="800" />
            <wire x2="800" y1="976" y2="1104" x1="800" />
            <wire x2="800" y1="1104" y2="1344" x1="800" />
            <wire x2="800" y1="1344" y2="1600" x1="800" />
            <wire x2="800" y1="1600" y2="1728" x1="800" />
        </branch>
        <bustap x2="896" y1="1104" y2="1104" x1="800" />
        <bustap x2="896" y1="1344" y2="1344" x1="800" />
        <bustap x2="896" y1="928" y2="928" x1="800" />
        <bustap x2="896" y1="976" y2="976" x1="800" />
        <branch name="CBus(2)">
            <wire x2="1104" y1="1104" y2="1104" x1="896" />
        </branch>
        <branch name="CBus(3)">
            <wire x2="1104" y1="1344" y2="1344" x1="896" />
        </branch>
        <branch name="CBus(4)">
            <wire x2="1680" y1="976" y2="976" x1="896" />
            <wire x2="1680" y1="976" y2="1184" x1="1680" />
            <wire x2="1792" y1="1184" y2="1184" x1="1680" />
        </branch>
        <branch name="CBus(5)">
            <wire x2="1728" y1="928" y2="928" x1="896" />
            <wire x2="1728" y1="928" y2="1120" x1="1728" />
            <wire x2="1792" y1="1120" y2="1120" x1="1728" />
        </branch>
        <iomarker fontsize="28" x="640" y="880" name="CBus(11:0)" orien="R180" />
        <branch name="CBus(11:8)">
            <wire x2="1680" y1="1600" y2="1600" x1="896" />
            <wire x2="1792" y1="1376" y2="1376" x1="1680" />
            <wire x2="1680" y1="1376" y2="1600" x1="1680" />
        </branch>
        <bustap x2="896" y1="1600" y2="1600" x1="800" />
        <instance x="944" y="1568" name="XLXI_4" orien="R0" />
        <branch name="XLXN_9">
            <wire x2="1104" y1="1168" y2="1168" x1="1008" />
            <wire x2="1008" y1="1168" y2="1408" x1="1008" />
            <wire x2="1104" y1="1408" y2="1408" x1="1008" />
            <wire x2="1008" y1="1408" y2="1440" x1="1008" />
        </branch>
    </sheet>
</drawing>