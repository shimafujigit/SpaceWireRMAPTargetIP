------------------------------------------------------------------------------
-- The MIT License (MIT)
--
-- Copyright (c) <2013> <Shimafuji Electric Inc., Osaka University, JAXA>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

library work;
use work.SpaceWireCODECIPPackage.all;
use work.RMAPTargetIPPackage.all;

entity RMAPTargetIP is
    generic (
        gBusWidth : integer range 8 to 32 := 32);  -- 8 = 8bit, 16 = 16bit, 32 = 32bit,
    port (
        clock : in std_logic;
        reset : in std_logic;

        transmitClock      : in  std_logic;
        receiveClock       : in  std_logic;
        --SpaceWire signals
        spaceWireDataIn    : in  std_logic;
        spaceWireStrobeIn  : in  std_logic;
        spaceWireDataOut   : out std_logic;
        spaceWireStrobeOut : out std_logic;

        --Internal BUS 
        busMasterCycleOut       : out std_logic;
        busMasterStrobeOut      : out std_logic;
        busMasterAddressOut     : out std_logic_vector (31 downto 0);
        busMasterByteEnableOut  : out std_logic_vector ((gBusWidth/8)-1 downto 0);
        busMasterDataIn         : in  std_logic_vector (gBusWidth-1 downto 0);
        busMasterDataOut        : out std_logic_vector (gBusWidth-1 downto 0);
        busMasterWriteEnableOut : out std_logic;
        busMasterReadEnableOut  : out std_logic;
        busMasterAcknowledgeIn  : in  std_logic;
        busMasterTimeOutErrorIn : in  std_logic;

        -- time code
        tickIn          : in  std_logic;
        timeIn          : in  std_logic_vector(5 downto 0);
        controlFlagsIn  : in  std_logic_vector(1 downto 0);
        tickOut         : out std_logic;
        timeOut         : out std_logic_vector (5 downto 0);
        controlFlagsOut : out std_logic_vector (1 downto 0);

        -- spw control                                  
        linkStart                : in  std_logic;
        linkDisable              : in  std_logic;
        autoStart                : in  std_logic;
        linkStatus               : out std_logic_vector (15 downto 0);
        errorStatus              : out std_logic_vector (7 downto 0);
        transmitClockDivideValue : in  std_logic_vector (5 downto 0);

        -- RMAP Statemachine state                                     
        commandStateOut : out commandStateMachine;
        replyStateOut   : out replyStateMachine;

        -- RMAP_User_Decode
        rmapLogicalAddressOut : out std_logic_vector(7 downto 0);
        rmapCommandOut        : out std_logic_vector(3 downto 0);
        rmapKeyOut            : out std_logic_vector(7 downto 0);
        rmapAddressOut        : out std_logic_vector(31 downto 0);
        rmapDataLengthOut     : out std_logic_vector(23 downto 0);
        requestAuthorization  : out std_logic;
        authorizeIn           : in  std_logic;
        rejectIn              : in  std_logic;
        replyStatusIn         : in  std_logic_vector(7 downto 0);

        -- RMAP Error Code and Status
        rmapErrorCode       : out std_logic_vector(7 downto 0);
        errorIndication     : out std_logic;
        writeDataIndication : out std_logic;
        readDataIndication  : out std_logic;
        rmwDataIndication   : out std_logic;

        -- statistics                                    
        statisticalInformationClear : in  std_logic;
        statisticalInformation      : out bit32X8Array
        );
end RMAPTargetIP;

architecture behavioral of RMAPTargetIP is

    component SpaceWireCODECIP is
        port (
            clock                       : in  std_logic;
            transmitClock               : in  std_logic;
            receiveClock                : in  std_logic;
            reset                       : in  std_logic;
--
            transmitFIFOWriteEnable     : in  std_logic;
            transmitFIFODataIn          : in  std_logic_vector (8 downto 0);
            transmitFIFOFull            : out std_logic;
            --transmitFIFODataCount   : out std_logic_vector (5 downto 0);
            receiveFIFOReadEnable       : in  std_logic;
            receiveFIFODataOut          : out std_logic_vector (8 downto 0);
            --receiveFIFOEmpty        : out std_logic; 
            --receiveFIFOFull         : out std_logic;
            receiveFIFODataCount        : out std_logic_vector (5 downto 0);
--
            tickIn                      : in  std_logic;
            timeIn                      : in  std_logic_vector (5 downto 0);
            controlFlagsIn              : in  std_logic_vector (1 downto 0);
            tickOut                     : out std_logic;
            timeOut                     : out std_logic_vector (5 downto 0);
            controlFlagsOut             : out std_logic_vector (1 downto 0);
--
            linkStart                   : in  std_logic;
            linkDisable                 : in  std_logic;
            autoStart                   : in  std_logic;
            linkStatus                  : out std_logic_vector(15 downto 0);
            errorStatus                 : out std_logic_vector (7 downto 0);
            transmitClockDivideValue    : in  std_logic_vector (5 downto 0);
            creditCount                 : out std_logic_vector(5 downto 0);
            outstandingCount            : out std_logic_vector(5 downto 0);
--          
            transmitActivity            : out std_logic;
            receiveActivity             : out std_logic;
--
            spaceWireDataOut            : out std_logic;
            spaceWireStrobeOut          : out std_logic;
            spaceWireDataIn             : in  std_logic;
            spaceWireStrobeIn           : in  std_logic;
--
            statisticalInformationClear : in  std_logic;
            statisticalInformation      : out bit32X8Array
            );
    end component;

    component RMAPTargetIPDecoder is
        generic (
            gBusWidth : integer range 8 to 32 := 32  -- 8 = 8bit, 16 = 16bit, 32 = 32bit,
            );
        port (
            clock                    : in  std_logic;
            reset                    : in  std_logic;
            -- FIFO
            transmitFIFOWriteEnable  : out std_logic;
            transmitFIFODataIn       : out std_logic_vector (8 downto 0);
            transmitFIFOFull         : in  std_logic;
            receiveFIFOReadEnable    : out std_logic;
            receiveFIFODataOut       : in  std_logic_vector (8 downto 0);
            receiveFIFODataCount     : in  std_logic_vector (5 downto 0);
            -- dma
            rmapAddress              : out std_logic_vector (31 downto 0);
            rmapDataLength           : out std_logic_vector (23 downto 0);
            dmaReadBufferWriteEnable : in  std_logic;
            dmaWriteBufferReadEnable : in  std_logic;
            dmaReadBufferWriteReady  : out std_logic;
            dmaReadBufferReadReady   : out std_logic;
            dmaReadBufferWriteData   : in  std_logic_vector (7 downto 0);
            dmaWriteBufferReadData   : out std_logic_vector (7 downto 0);
            dmaWriteBufferEmpty      : out std_logic;
            rmapCommand              : out std_logic_vector (3 downto 0);
            busAccessStart           : out std_logic;
            busAccessStop            : out std_logic;
            busAccessEnd             : in  std_logic;
            busMasterTimeOutErrorIn  : in  std_logic;
            -- RMAP State                                                 
            commandStateOut          : out commandStateMachine;
            replyStateOut            : out replyStateMachine;
            -- RMAP_User_Decode
            rmapLogicalAddressOut    : out std_logic_vector(7 downto 0);
            rmapKeyOut               : out std_logic_vector(7 downto 0);
            -- RMAP Transaction control
            requestAuthorization     : out std_logic;
            authorizeIn              : in  std_logic;
            rejectIn                 : in  std_logic;
            replyStatusIn            : in  std_logic_vector(7 downto 0);
            -- RMAP Error Code and Status
            rmapErrorCode            : out std_logic_vector(7 downto 0);
            errorIndication          : out std_logic;
            writeDataIndication      : out std_logic;
            readDataIndication       : out std_logic;
            rmwDataIndication        : out std_logic
            );
    end component;

    component RMAPTargetIPDMAController is
        generic (
            gBusWidth : integer range 8 to 32 := 32  -- 8 = 8bit, 16 = 16bit, 32 = 32bit,
            );
        port (
            clock                    : in  std_logic;
            reset                    : in  std_logic;
            -- dma i/f
            rmapAddress              : in  std_logic_vector (31 downto 0);
            rmapDataLength           : in  std_logic_vector (23 downto 0);
            dmaReadBufferWriteEnable : out std_logic;
            dmaWriteBufferReadEnable : out std_logic;
            dmaReadBufferWriteReady  : in  std_logic;
            dmaReadBufferWriteData   : out std_logic_vector (7 downto 0);
            dmaWriteBufferReadData   : in  std_logic_vector (7 downto 0);
            dmaWriteBufferEmpty      : in  std_logic;
            rmapCommand              : in  std_logic_vector (3 downto 0);
            busAccessStart           : in  std_logic;
            busAccessStop            : in  std_logic;
            busAccessEnd             : out std_logic;
            busMasterTimeOutErrorIn  : in  std_logic;
            -- bus i/f
            busMasterCycleOut        : out std_logic;
            busMasterStrobeOut       : out std_logic;
            busMasterAddressOut      : out std_logic_vector (31 downto 0);
            busMasterByteEnableOut   : out std_logic_vector (3 downto 0);
            busMasterDataIn          : in  std_logic_vector (31 downto 0);
            busMasterDataOut         : out std_logic_vector (31 downto 0);
            busMasterWriteEnableOut  : out std_logic;
            busMasterReadEnableOut   : out std_logic;
            busMasterAcknowledgeIn   : in  std_logic
            );
    end component;
    --
    signal transmitFIFOWriteEnable      : std_logic;
    signal transmitFIFODataIn           : std_logic_vector (8 downto 0);
    signal transmitFIFOFull             : std_logic;
    signal receiveFIFOReadEnable        : std_logic;
    signal receiveFIFODataOut           : std_logic_vector (8 downto 0);
    signal receiveFIFODataCount         : std_logic_vector (5 downto 0);
    --
    signal rmapAddress                  : std_logic_vector (31 downto 0);
    signal dmaReadBufferWriteEnable     : std_logic;
    signal dmaWriteBufferReadEnable     : std_logic;
    signal dmaReadBufferWriteReady      : std_logic;
    signal rmapDataLength               : std_logic_vector (23 downto 0);
    signal dmaReadBufferWriteData       : std_logic_vector (7 downto 0);
    signal dmaWriteBufferReadData       : std_logic_vector (7 downto 0);
    signal dmaWriteBufferEmpty          : std_logic;
    signal rmapCommand                  : std_logic_vector (3 downto 0);
    signal busAccessStart               : std_logic;
    signal busAccessStop                : std_logic;
    signal busAccessEnd                 : std_logic;
    --
    signal busMasterByteEnableOutSignal : std_logic_vector (3 downto 0);
    signal busMasterDataOutSignal       : std_logic_vector (31 downto 0);
    signal iBusMasterDataIn             : std_logic_vector (31 downto 0);
    
    
begin
    spaceWireCODEC : SpaceWireCODECIP
        port map (
            clock                       => clock,
            transmitClock               => transmitClock,
            receiveClock                => receiveClock,
            reset                       => reset,
            -- SpaceWireCODEC Data i/f
            transmitFIFOWriteEnable     => transmitFIFOWriteEnable,
            transmitFIFODataIn          => transmitFIFODataIn,
            transmitFIFOFull            => transmitFIFOFull,
            --transmitFIFODataCount     => open,
            receiveFIFOReadEnable       => receiveFIFOReadEnable,
            receiveFIFODataOut          => receiveFIFODataOut,
            --receiveFIFOFull           => open,
            --receiveFIFOEmpty          => open,
            receiveFIFODataCount        => receiveFIFODataCount,
            -- time code
            tickIn                      => tickIn,
            timeIn                      => timeIn,
            controlFlagsIn              => controlFlagsIn,
            tickOut                     => tickOut,
            timeOut                     => timeOut,
            controlFlagsOut             => controlFlagsOut,
            -- spw control
            linkStart                   => linkStart,
            linkDisable                 => linkDisable,
            autoStart                   => autoStart,
            linkStatus                  => linkStatus,
            errorStatus                 => errorStatus,
            transmitClockDivideValue    => transmitClockDivideValue,
            creditCount                 => open,
            outstandingCount            => open,
            -- serial i/o        
            spaceWireDataOut            => spaceWireDataOut,
            spaceWireStrobeOut          => spaceWireStrobeOut,
            spaceWireDataIn             => spaceWireDataIn,
            spaceWireStrobeIn           => spaceWireStrobeIn,
            -- statistics            
            statisticalInformationClear => statisticalInformationClear,
            statisticalInformation      => statisticalInformation
            );

    RMAPDecoder : RMAPTargetIPDecoder
        generic map (gBusWidth => gBusWidth)
        port map (
            clock                    => clock,
            reset                    => reset,
            -- fifo
            transmitFIFOWriteEnable  => transmitFIFOWriteEnable,
            transmitFIFODataIn       => transmitFIFODataIn,
            transmitFIFOFull         => transmitFIFOFull,
            receiveFIFOReadEnable    => receiveFIFOReadEnable,
            receiveFIFODataOut       => receiveFIFODataOut,
            receiveFIFODataCount     => receiveFIFODataCount,
            -- dma
            rmapAddress              => rmapAddress,
            rmapDataLength           => rmapDataLength,
            dmaReadBufferWriteEnable => dmaReadBufferWriteEnable,
            dmaWriteBufferReadEnable => dmaWriteBufferReadEnable,
            dmaReadBufferWriteReady  => dmaReadBufferWriteReady,
            dmaReadBufferReadReady   => open,
            dmaReadBufferWriteData   => dmaReadBufferWriteData,
            dmaWriteBufferReadData   => dmaWriteBufferReadData,
            dmaWriteBufferEmpty      => dmaWriteBufferEmpty,
            rmapCommand              => rmapCommand,
            busAccessStart           => busAccessStart,
            busAccessStop            => busAccessStop,
            busAccessEnd             => busAccessEnd,
            busMasterTimeOutErrorIn  => busMasterTimeOutErrorIn,
            -- RMAP State       
            commandStateOut          => commandStateOut,
            replyStateOut            => replyStateOut,
            -- RMAP_User_Decode               
            rmapLogicalAddressOut    => rmapLogicalAddressOut,
            rmapKeyOut               => rmapKeyOut,
            -- RMAP Transaction control
            requestAuthorization     => requestAuthorization,
            authorizeIn              => authorizeIn,
            rejectIn                 => rejectIn,
            replyStatusIn            => replyStatusIn,
            -- RMAP Error Code and Status
            rmapErrorCode            => rmapErrorCode,
            errorIndication          => errorIndication,
            writeDataIndication      => writeDataIndication,
            readDataIndication       => readDataIndication,
            rmwDataIndication        => rmwDataIndication
            );

    rmapCommandOut    <= rmapCommand;
    rmapAddressOut    <= rmapAddress;
    rmapDataLengthOut <= rmapDataLength;


    RMAPDMAController : RMAPTargetIPDMAController
        generic map (gBusWidth => gBusWidth)
        port map (
            clock                    => clock,
            reset                    => reset,
            -- dma i/f
            rmapAddress              => rmapAddress,
            rmapDataLength           => rmapDataLength,
            dmaReadBufferWriteEnable => dmaReadBufferWriteEnable,
            dmaWriteBufferReadEnable => dmaWriteBufferReadEnable,
            dmaReadBufferWriteReady  => dmaReadBufferWriteReady,
            dmaReadBufferWriteData   => dmaReadBufferWriteData,
            dmaWriteBufferReadData   => dmaWriteBufferReadData,
            dmaWriteBufferEmpty      => dmaWriteBufferEmpty,
            rmapCommand              => rmapCommand,
            busAccessStart           => busAccessStart,
            busAccessStop            => busAccessStop,
            busAccessEnd             => busAccessEnd,
            -- bus i/f
            busMasterCycleOut        => busMasterCycleOut,
            busMasterStrobeOut       => busMasterStrobeOut,
            busMasterAddressOut      => busMasterAddressOut,
            busMasterByteEnableOut   => busMasterByteEnableOutSignal,
            busMasterDataIn          => iBusMasterDataIn,
            busMasterDataOut         => busMasterDataOutSignal,
            busMasterWriteEnableOut  => busMasterWriteEnableOut,
            busMasterReadEnableOut   => busMasterReadEnableOut,
            busMasterAcknowledgeIn   => busMasterAcknowledgeIn,
            busMasterTimeOutErrorIn  => busMasterTimeOutErrorIn
            );


    BusWidth8 : if (gBusWidth = 8) generate
        iBusMasterDataIn       <= x"000000" & busMasterDataIn(7 downto 0);
        busMasterDataOut       <= busMasterDataOutSignal(7 downto 0);
        busMasterByteEnableOut <= busMasterByteEnableOutSignal(0 downto 0);
    end generate;

    BusWidth16 : if (gBusWidth = 16) generate
        iBusMasterDataIn       <= x"0000" & busMasterDataIn(15 downto 0);
        busMasterDataOut       <= busMasterDataOutSignal(15 downto 0);
        busMasterByteEnableOut <= busMasterByteEnableOutSignal(1 downto 0);
    end generate;

    BusWidth32 : if (gBusWidth = 32) generate
        iBusMasterDataIn       <= busMasterDataIn;
        busMasterDataOut       <= busMasterDataOutSignal;
        busMasterByteEnableOut <= busMasterByteEnableOutSignal;
    end generate;
    

end behavioral;
