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

entity RMAPTargetIPDecoder is
    generic (
        gBusWidth : integer range 8 to 32 := 32);  -- 8 = 8bit, 16 = 16bit, 32 = 32bit,
    port (
        clock                    : in  std_logic;
        reset                    : in  std_logic;
        -- fifo
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
        dmaReadBufferReadReady   : out std_logic;  --not use
        dmaReadBufferWriteData   : in  std_logic_vector (7 downto 0);
        dmaWriteBufferReadData   : out std_logic_vector (7 downto 0);
        dmaWriteBufferEmpty      : out std_logic;
        rmapCommand              : out std_logic_vector (3 downto 0);
        busAccessStart           : out std_logic;
        busAccessEnd             : in  std_logic;
        busAccessStop            : out std_logic;
        busMasterTimeOutErrorIn  : in  std_logic;
        -- RMAP State                                                 
        commandStateOut          : out commandStateMachine;
        replyStateOut            : out replyStateMachine;
        -- RMAP Transaction information                               
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
end RMAPTargetIPDecoder;

architecture behavioral of RMAPTargetIPDecoder is
    
    constant cRMAPProtocolID : std_logic_vector (7 downto 0) := "00000001";
    
    type receiveFIFOStateMachine is (
        receiveFIFOStateIdle,
        receiveFIFOStateRead0,
        receiveFIFOStateRead1,
        receiveFIFOStateRead2,
        receiveFIFOStateRead3,
        receiveFIFOStateRead4
        );
    signal receiveFIFOState : receiveFIFOStateMachine;
    
    type transmitFIFOStateMachine is (
        transmitFIFOStateIdle,
        transmitFIFOStateWrite0,
        transmitFIFOStateWrite1,
        transmitFIFOStateWrite2
        );
    signal transmitFIFOState : transmitFIFOStateMachine;

    signal commandState : commandStateMachine;
    signal replyState   : replyStateMachine;

    signal iReceiveControlFlag : std_logic;
    signal iReceiveData        : std_logic_vector (7 downto 0);
    signal iReceiveFIFOReady   : std_logic;

    signal iRMAPCommand               : std_logic_vector (3 downto 0);
    signal iRMAPCommand2              : std_logic_vector (3 downto 0);
    signal iReplyAddressSize          : std_logic_vector (1 downto 0);
    signal iReplyAddressSize2         : std_logic_vector (1 downto 0);
    signal iReplyAddressFieldCount    : std_logic_vector (1 downto 0);
    signal iReplyAddressAvailable     : std_logic;
    signal iReplyAddressFieldReceive  : integer range 0 to 12 := 0;
    signal iReplyAddressFieldTransmit : integer range 0 to 12 := 0;
    type   iRegisterArray is array (0 to 11) of std_logic_vector (7 downto 0);
    signal iReplyAddress              : iRegisterArray;
    signal iReplyAddress2             : iRegisterArray;
    signal iRMAPInvalidCommand        : std_logic;
    signal iTargetLogicalAddress      : std_logic_vector(7 downto 0);
    signal iRmapKey                   : std_logic_vector(7 downto 0);
    signal iInitiatorLogicalAddress   : std_logic_vector (7 downto 0);
    signal iTransactionID             : std_logic_vector (15 downto 0);
    signal iTransactionIdentifieField : std_logic;
    signal iExtendedAddress           : std_logic_vector (7 downto 0);
    signal iRMAPAddress               : std_logic_vector (31 downto 0);
    signal iMemoryAddressField        : std_logic_vector (1 downto 0);
    signal iRMAPDataLength            : std_logic_vector (23 downto 0);
    signal iDataLengthField           : std_logic_vector (1 downto 0);
    signal iDataCount                 : std_logic_vector (23 downto 0);
    signal iBusAccessStart            : std_logic;
    signal iBusAccessStarted          : std_logic;
    signal iBusAccessStop             : std_logic;
    signal iBusWriteMemoryError       : std_logic;
    signal iBusReadMemoryError        : std_logic;
    signal iReplyStart                : std_logic;
    signal iReplyProcessing           : std_logic;
    signal iErrorCode                 : std_logic_vector (7 downto 0);
    signal iErrorReply                : std_logic;
    signal iAfterHeaderCRC            : std_logic;  --Immediately after the complete header including header CRC.
    signal iErrorIndication           : std_logic;
    signal iWriteDataIndication       : std_logic;
    signal iReadDataIndication        : std_logic;
    signal iRMWDataIndication         : std_logic;
    signal iIdleState                 : std_logic;
    signal iAuthorized                : std_logic;

    
    signal iTransmitFIFOReady       : std_logic;
    signal iTransmitFIFOWriteEnable : std_logic;
    signal iTransmitFIFODataIn      : std_logic_vector (8 downto 0);
    signal iCRCCalculatePhase       : std_logic;

    signal iCRCByteCalculated        : std_logic;
    signal iReplyTransactionIDField  : std_logic;
    signal iReplyDataLengthField     : std_logic_vector (1 downto 0);
    signal iReplyDataLength          : std_logic_vector (23 downto 0);
    signal iReplyDataCount           : std_logic_vector (23 downto 0);
    signal iReplyDataSet             : std_logic;
    signal iReplySpaceWireAddressSet : std_logic;

    component RMAPTargetIPBufferFIFO8x2kXilinx is
        port (
            clock       : in  std_logic;
            dataIn      : in  std_logic_vector (7 downto 0);
            readEnable  : in  std_logic;
            reset       : in  std_logic;
            writeEnable : in  std_logic;
            dataCount   : out std_logic_vector (10 downto 0);
            dataOut     : out std_logic_vector (7 downto 0);
            empty       : out std_logic;
            full        : out std_logic
            );
    end component;

    component RMAPTargetIPBufferFIFO8x2kAltera is
        port (
            clock       : in  std_logic;
            dataIn      : in  std_logic_vector (7 downto 0);
            readEnable  : in  std_logic;
            reset       : in  std_logic;
            writeEnable : in  std_logic;
            dataCount   : out std_logic_vector (10 downto 0);
            dataOut     : out std_logic_vector (7 downto 0);
            empty       : out std_logic;
            full        : out std_logic
            );
    end component;

    signal iDMAReadBufferReadReady   : std_logic;
    signal iReadBufferReady          : std_logic;
    signal iReadBufferReadEnable     : std_logic;
    signal readBufferReadData        : std_logic_vector (7 downto 0);
    signal iDMAReadBufferWriteReady  : std_logic;
    signal iDMAReadBufferWriteEnable : std_logic;
    signal iDMAReadBufferWriteData   : std_logic_vector (7 downto 0);
    signal readBufferDataCount       : std_logic_vector (10 downto 0);
    signal readBufferFull            : std_logic;
    signal readBufferEmpty           : std_logic;
    signal iReadBufferClear          : std_logic;
    signal iReadBufferReset          : std_logic;

    signal iWriteBufferWriteEnable : std_logic;
    signal writeBufferDataCount    : std_logic_vector (10 downto 0);
    signal writeBufferFull         : std_logic;
    signal iWriteBufferClear       : std_logic;
    signal iWriteBufferReset       : std_logic;

    component RMAPTargetIPCRCRomXilinx is
        port (
            clock   : in  std_logic;
            address : in  std_logic_vector (8 downto 0);
            dataOut : out std_logic_vector (7 downto 0)
            );
    end component;

    component RMAPTargetIPCRCRomAltera is
        port (
            clock   : in  std_logic;
            address : in  std_logic_vector (8 downto 0);
            dataOut : out std_logic_vector (7 downto 0)
            );
    end component;

    signal iReceiveFIFOReadEnable      : std_logic;
    signal iCommandCRC                 : std_logic_vector (7 downto 0);
    signal iCommandCRCRomAddressBuffer : std_logic_vector (7 downto 0);
    signal commandCRCRomOut            : std_logic_vector (7 downto 0);
    signal iReceiveFIFODataReady       : std_logic;
    signal iCommandCRCRomAddress       : std_logic_vector (8 downto 0);

    signal iReplyCRCOut           : std_logic_vector (7 downto 0);
    signal iReplyCRCAddressBuffer : std_logic_vector (7 downto 0);
    signal replyCRCRomOut         : std_logic_vector (7 downto 0);
    signal iReplyCRCRomAddress    : std_logic_vector (8 downto 0);

    signal iCommandCheck            : std_logic;
    signal iCommandCheckOK          : std_logic;
    signal iCommandCheckNG          : std_logic;
    signal iRequestAuthorization    : std_logic;
    signal iDataLength              : std_logic_vector (23 downto 0);
    signal iReadModifyWriteLengthNG : std_logic;

begin
    dmaReadBufferWriteReady <= iDMAReadBufferWriteReady;
    dmaReadBufferReadReady  <= iDMAReadBufferReadReady;

    transmitFIFOWriteEnable <= iTransmitFIFOWriteEnable;
    transmitFIFODataIn      <= iTransmitFIFODataIn;
    iTransmitFIFOReady      <= '1' when transmitFIFOFull = '0'          else '0';
    iReceiveFIFOReady       <= '0' when receiveFIFODataCount = "000000" else '1';
    receiveFIFOReadEnable   <= iReceiveFIFOReadEnable;

    busAccessStart            <= iBusAccessStart;
    busAccessStop             <= iBusAccessStop;
    idmaReadBufferWriteReady  <= '0' when readBufferFull = '1'  else '1';
    idmaReadBufferReadReady   <= '0' when readBufferEmpty = '1' else '1';
    iDMAReadBufferWriteEnable <= dmaReadBufferWriteEnable;
    iReadBufferReady          <= '1' when readBufferEmpty = '0' else '0';

    iRMAPInvalidCommand <= '1' when ((iRMAPCommand(3) = '0' and iRMAPCommand(1) = '0') or iRMAPCommand = "0110") else '0';

    ----------------------------------------------------------------------
    -- latch the RMAP Header information before deliver to the user module.
    ----------------------------------------------------------------------
    process (clock, reset)
    begin
        if (reset = '1') then
            rmapAddress           <= (others => '0');
            rmapDataLength        <= (others => '0');
            rmapCommand           <= (others => '0');
            rmapLogicalAddressOut <= (others => '0');
            rmapKeyOut            <= (others => '0');
            --
            requestAuthorization  <= '0';
            --
            commandStateOut       <= commandStateIdle;
            replyStateOut         <= replyStateIdle;
            rmapErrorCode         <= x"00";
            
        elsif (clock'event and clock = '1') then
            rmapAddress           <= iRMAPAddress;
            rmapDataLength        <= iRMAPDataLength;
            rmapCommand           <= iRMAPCommand;
            rmapLogicalAddressOut <= iTargetLogicalAddress;
            rmapKeyOut            <= iRmapKey;
            --
            requestAuthorization  <= iRequestAuthorization;
            --
            commandStateOut       <= commandState;
            replyStateOut         <= replyState;
            rmapErrorCode         <= iErrorCode;
        end if;
    end process;




    ----------------------------------------------------------------------
    --  ECSS-E-ST-50-11C 6.4.3.7 Write data indication
    --  ECSS-E-ST-50-11C 6.5.3.7 Read data indication
    --  ECSS-E-ST-50-11C 6.6.3.8 Read-modify write data indication
    ----------------------------------------------------------------------
    errorIndication     <= iErrorIndication;
    writeDataIndication <= iWriteDataIndication;
    readDataIndication  <= iReadDataIndication;
    rmwDataIndication   <= iRMWDataIndication;
    
    process (clock, reset)
    begin
        if (reset = '1') then
            iErrorIndication     <= '0';
            iWriteDataIndication <= '0';
            iReadDataIndication  <= '0';
            iRMWDataIndication   <= '0';
            iAuthorized          <= '0';
            
        elsif (clock'event and clock = '1') then
        
            if (busAccessEnd = '1') then
                case iRMAPCommand(3 downto 2) is
                
                when "00" => --Read
                    iReadDataIndication  <= '1';
                    
                when "01" => --ReMW
                    iRMWDataIndication   <= '1';
                    
                when "10" | "11" => --Write
                    iWriteDataIndication <= '1';
                
                when others =>
                    iWriteDataIndication <= '0';
                    iReadDataIndication  <= '0';
                    iRMWDataIndication   <= '0';
                end case;
                
            else
                iWriteDataIndication <= '0';
                iReadDataIndication  <= '0';
                iRMWDataIndication   <= '0';
            end if;
            -----------------------------------------------
            -- Error Indication
            -----------------------------------------------
            if (commandState = commandStateDataEOP or commandState = commandStateData) then
                iAuthorized <= '1';
            end if;
            
            if (commandState = commandStateIdle) then
                iIdleState  <= '1';
                iAuthorized <= '0';
                if (iIdleState = '0' and iAuthorized = '1' and iErrorCode /= x"00") then
                    iErrorIndication <= '1';
                end if;
            else
                iIdleState <= '0';
            end if;
            
            if (iErrorIndication = '1') then
                iErrorIndication <= '0';
            end if;
            
        end if;
    end process;
    
    
    ----------------------------------------------------------------------
    -- Determine the combination of the data length and address from received 
    -- RMAP header could handle by the RMAP Target IP core.
    -- If it could handle, output iCommandCheckOK signal.
    -- Then output the iRequestAuthorization signal to the user-side module
    -- to obtain access to the internal bus.
    ----------------------------------------------------------------------
    iDataLength <= ('0' & iRMAPDataLength(23 downto 1))when iRMAPCommand = "0111" else iRMAPDataLength;

    process (clock, reset)
    begin
        if (reset = '1') then
            iCommandCheckOK          <= '0';
            iCommandCheckNG          <= '0';
            iRequestAuthorization    <= '0';
            iReadModifyWriteLengthNG <= '0';
            
        elsif (clock'event and clock = '1') then

            ----------------------------------------------------------------------
            -- If RMAP packet can be handle with the RMAP Target IP core, 
            -- request the access permission to user module.
            ----------------------------------------------------------------------
            if (commandState = commandStateHeaderCRC) then
                if (iCommandCheckOK = '1') then
                    iRequestAuthorization <= '1';
                end if;
            else
                iRequestAuthorization <= '0';
            end if;

            ----------------------------------------------------------------------
            --  ECSS-E-ST-50-11C 6.6.1.12 Data Length field
            -- Judgement of the Read_Modify_Write data length.
            ----------------------------------------------------------------------
            if (iRMAPCommand (3 downto 0) = "0111") then
                if (iRMAPDataLength = 8 or iRMAPDataLength = 6 or
                    iRMAPDataLength = 4 or iRMAPDataLength = 2) then
                    iReadModifyWriteLengthNG <= '0';
                else
                    iReadModifyWriteLengthNG <= '1';
                end if;
            else
                iReadModifyWriteLengthNG <= '0';
            end if;

            ----------------------------------------------------------------------
            -- Judge the data length can be handled.
            ----------------------------------------------------------------------
            if (iCommandCheck = '1') then
                
                if (gBusWidth = 8) then  --8bit Bus 
                    iCommandCheckOK <= '1';
                    iCommandCheckNG <= '0';
                end if;

                if (gBusWidth = 16) then  --16bit Bus               
                    if (iDataLength = x"000000")then     --0byte Access
                        iCommandCheckNG <= '1';
                    elsif (iDataLength = x"000001")then  --1byte Access
                        iCommandCheckOK <= '1';
                    elsif (iDataLength(0) = '0') then    --(2*N)byte Access
                        if (iRMAPAddress(0) = '0') then
                            iCommandCheckOK <= '1';
                        else
                            iCommandCheckNG <= '1';
                        end if;
                    else
                        iCommandCheckNG <= '1';
                    end if;
                end if;

                if (gBusWidth = 32) then  --32bit Bus                     
                    if (iDataLength = x"000000")then             --0byte Access
                        iCommandCheckNG <= '1';
                    elsif (iDataLength = x"000001")then          --1byte Access
                        iCommandCheckOK <= '1';
                    elsif (iDataLength = x"000002")then          --2byte Access
                        if (iRMAPAddress(0) = '0') then
                            iCommandCheckOK <= '1';
                        else
                            iCommandCheckNG <= '1';
                        end if;
                    elsif (iDataLength(1 downto 0) = "00") then  --(4*N)byte Access
                        if (iRMAPAddress(1 downto 0) = "00") then
                            iCommandCheckOK <= '1';
                        else
                            iCommandCheckNG <= '1';
                        end if;
                    else  --3byte, (4*N)+1byte, (4*N)+2byte, (4*N)+3byte
                        iCommandCheckNG <= '1';
                    end if;
                end if;
            else
                iCommandCheckOK <= '0';
                iCommandCheckNG <= '0';
            end if;
        end if;
    end process;



    ----------------------------------------------------------------------
    --  ECSS-E-ST-50-11C 6 Remote memory access protocol
    -- ECSS-E-ST-50-11C 6.4.3 Write command format
    -- ECSS-E-ST-50-11C 6.5.3 Read command format
    -- ECSS-E-ST-50-11C 6.6.3 Read-modify-write command format
    -- ECSS-E-ST-50-11C 6.7 Error and status codes
    --
    -- Interpret command instruction sequentially.
    ----------------------------------------------------------------------
    process (clock, reset)
    begin
        if (reset = '1') then
            commandState               <= commandStateIdle;
            iRMAPCommand               <= (others => '0');
            iRMAPCommand2              <= (others => '0');
            iReplyAddressSize          <= (others => '0');
            iReplyAddressSize2         <= (others => '0');
            iInitiatorLogicalAddress   <= (others => '0');
            iReplyAddressFieldCount    <= (others => '0');
            iReplyAddressAvailable     <= '0';
            iReplyAddressFieldReceive  <= 0;
            iReplyAddress              <= (others => (others => '0'));
            iReplyAddress2             <= (others => (others => '0'));
            iTargetLogicalAddress      <= (others => '0');
            iRmapKey                   <= (others => '0');
            iTransactionID             <= (others => '0');
            iTransactionIdentifieField <= '0';
            iExtendedAddress           <= (others => '0');
            iRMAPAddress               <= (others => '0');
            iMemoryAddressField        <= (others => '0');
            iRMAPDataLength            <= (others => '0');
            iDataLengthField           <= (others => '0');
            iWriteBufferWriteEnable    <= '0';
            iWriteBufferClear          <= '0';
            iDataCount                 <= (others => '0');
            iBusAccessStart            <= '0';
            iBusAccessStarted          <= '0';
            iBusAccessStop             <= '0';
            iBusWriteMemoryError       <= '0';
            iErrorCode                 <= (others => '0');
            iErrorReply                <= '0';
            iReplyStart                <= '0';
            iAfterHeaderCRC            <= '0';
            --
            iCommandCheck              <= '0';

        elsif (clock'event and clock = '1') then

            ----------------------------------------------------------------------
            --  ECSS-E-ST-50-11C 6.4.3.10 Write not OK
            --  ECSS-E-ST-50-11C 6.6.3.11 Read and Write not OK
            -- Send error reply when detect bus time out error while write operations to 
            -- internal bus.
            -- Internal bus access will stop.
            ----------------------------------------------------------------------
            if (busMasterTimeOutErrorIn = '1') then
                iBusWriteMemoryError <= '1';
            elsif (commandState = commandStateIdle) then
                iBusWriteMemoryError <= '0';
            end if;

            if (busAccessEnd = '1' or busMasterTimeOutErrorIn = '1') then
                iBusAccessStarted <= '0';
            end if;

            ----------------------------------------------------------------------
            -- Wait until finished accessing to internal bus.
            ----------------------------------------------------------------------
            if (commandState = commandStateWaitBusAccessEnd) then
                iBusAccessStart <= '0';

                if (busMasterTimeOutErrorIn = '1') then
                    iWriteBufferClear <= '1';
                    iErrorReply       <= '1';
                    iErrorCode        <= x"01";
                elsif (iBusAccessStarted = '0') then
                    iReplyStart  <= '1';
                    commandState <= commandStateIdle;
                end if;
            end if;

            ----------------------------------------------------------------------
            -- Waiting to receive the RMAP packet.
            -- The statemchine transit to the Target Logical Address state when ReceiveFIFO
            -- can be readable.
            ----------------------------------------------------------------------
            if (commandState = commandStateIdle) then
                iReplyStart       <= '0';
                iCommandCheck     <= '0';
                iWriteBufferClear <= '0';

                if (iReceiveFIFOReadEnable = '1') then
                    iErrorCode   <= x"00";
                    commandState <= commandStateTargetLogicalAddress;
                end if;
            end if;

            ----------------------------------------------------------------------
            -- Every single read of data from the receiveFIFO,
            -- it move statemachine which is command field to next state.
            ----------------------------------------------------------------------
            if (iReceiveFIFODataReady = '1') then
                case commandState is

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.2 Target Logical Address field
                    -- Store the received data in the logical address register. 
                    -- Evaluation is performed in the user module.
                    ----------------------------------------------------------------------
                    when commandStateTargetLogicalAddress =>
                        -- save LogicalAddress for later use
                        if (iReceiveControlFlag = '0') then
                            iTargetLogicalAddress <= iReceiveData;
                            commandState          <= commandStateProtocolID;
                        else
                            -- no cargo packet
                            if (iReceiveData(0) = '0') then  -- Early EOP                              
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.3 Protocol Identifier field
                    -- Compare the received data and the Protocol Identifier.
                    -- RMAP (0x01) only Correspondence.
                    ----------------------------------------------------------------------
                    when commandStateProtocolID =>
                        -- check protocol id is RMAP.
                        if (iReceiveControlFlag = '0') then
                            if (iReceiveData = cRMAPProtocolID) then
                                commandState <= commandStateInstruction;
                            else
                                -- if not RMAP, stop processing
                                commandState <= commandStateWaitEOP;
                            end if;
                            
                        else
                            -- Incomplete packet (too short to interpret)
                            if (iReceiveData(0) = '0') then  -- Early EOP                              
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.4 Instruction field
                    -- Store the received data into the iRMAPCommand register and iReplyAddressSize
                    -- register. Supported Packet type is only command_packet_type("01"), other 
                    -- paket type will be an error.
                    ----------------------------------------------------------------------
                    when commandStateInstruction =>
                        -- save cmd for later use
                        if (iReceiveControlFlag = '0') then
                            iRMAPCommand              <= iReceiveData (5 downto 2);
                            iRMAPCommand2             <= iReceiveData (5 downto 2);
                            iReplyAddressSize         <= iReceiveData (1 downto 0);
                            iReplyAddressSize2        <= iReceiveData (1 downto 0);
                            iReplyAddressFieldCount   <= (others => '0');
                            iReplyAddressAvailable    <= '0';
                            iReplyAddressFieldReceive <= 0;

                            if (iReceiveData (7 downto 6) = "01") then
                                -- command packet
                                commandState <= commandStateKey;
                            else
                                -- unused RMAP Packet or Reply Packet
                                iErrorCode <= x"02";
                                if (iReceiveData(3) = '1') then
                                    commandState <= commandStateKey;  -- ack=1 then continue
                                else
                                    commandState <= commandStateWaitEOP;  -- else discard
                                end if;
                                
                            end if;
                        else
                            -- Incomplete packet (too short to interpret)
                            if (iReceiveData(0) = '0') then  -- Early EOP                              
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.5 Key field
                    -- Store the received data into the RMAPKey register.
                    -- Evaluation is performed in the user module.
                    ----------------------------------------------------------------------
                    when commandStateKey =>
                        -- save Key for later use
                        if (iReceiveControlFlag = '0') then
                            iRmapKey <= iReceiveData;

                            if (iReplyAddressSize = "00") then
                                commandState <= commandStateInitiatorLogicalAddress;
                            else
                                commandState <= commandStateReplyAddress;
                            end if;
                            
                        else
                            -- Incomplete packet (too short to interpret)
                            if (iReceiveData(0) = '0') then  -- Early EOP                              
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.6 Reply Address field
                    -- Store the received data into the ReplyAddress register.
                    -- If there is no reply address, skip this State.
                    ----------------------------------------------------------------------
                    when commandStateReplyAddress =>
                        -- if source require path address save path address
                        if (iReceiveControlFlag = '0') then
                            if (iReplyAddressFieldCount = "11") then
                                iReplyAddressSize       <= iReplyAddressSize - 1;  --Size of Reply Address field
                                iReplyAddressFieldCount <= "00";

                                if (iReplyAddressSize = "01") then
                                    commandState <= commandStateInitiatorLogicalAddress;
                                end if;
                                
                            else
                                iReplyAddressFieldCount <= iReplyAddressFieldCount + 1;
                            end if;


                            if (iReplyAddressAvailable = '1') then
                                iReplyAddress(iReplyAddressFieldReceive) <= iReceiveData;
                                iReplyAddressFieldReceive                <= iReplyAddressFieldReceive + 1;

                            elsif (iReceiveData /= x"00") then
                                -- Leading bytes with the value 0x00 in the Reply Address field shall beignored.              
                                iReplyAddressAvailable                   <= '1';
                                iReplyAddress(iReplyAddressFieldReceive) <= iReceiveData;
                                iReplyAddressFieldReceive                <= iReplyAddressFieldReceive + 1;
                            end if;

                        else
                            -- Incomplete packet (too short to interpret)
                            if (iReceiveData(0) = '0') then  -- Early EOP                              
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.7 Initiator Logical Address field
                    -- Store the received data into the Initiator Logical Address register.
                    ----------------------------------------------------------------------
                    when commandStateInitiatorLogicalAddress =>
                        -- save source logical address                    
                        iReplyAddress2 <= iReplyAddress;  --Duplicate for later use at replyState.
                        if (iReceiveControlFlag = '0') then
                            iInitiatorLogicalAddress <= iReceiveData;
                            commandState             <= commandStateTransactionID;
                        else
                            -- Incomplete packet (too short to interpret)
                            if (iReceiveData(0) = '0') then  -- Early EOP
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.8 Transaction Identifier field
                    -- Store the received data into the Transaction ID register.
                    ----------------------------------------------------------------------
                    when commandStateTransactionID =>
                        -- save TransactionID for later use
                        if (iReceiveControlFlag = '0') then
                            if (iTransactionIdentifieField = '0') then
                                iTransactionID(15 downto 8) <= iReceiveData;
                                iTransactionIdentifieField  <= '1';
                            else
                                iTransactionID(7 downto 0) <= iReceiveData;
                                iTransactionIdentifieField <= '0';
                                commandState               <= commandStateExtendedAddress;
                            end if;
                        else
                            -- Incomplete packet (too short to interpret)
                            iTransactionIdentifieField <= '0';
                            if (iReceiveData(0) = '0') then  -- Early EOP                              
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.9 Extended Address field
                    -- Store the received data into the Extended Address register.
                    ----------------------------------------------------------------------
                    when commandStateExtendedAddress =>
                        -- save extended address
                        if (iReceiveControlFlag = '0') then
                            iExtendedAddress <= iReceiveData;
                            commandState     <= commandStateAddress;
                        else
                            -- Incomplete packet (too short to interpret)
                            if (iReceiveData(0) = '0') then  -- Early EOP                              
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.10 Address field
                    -- Store the received data into the RMAP Address register.
                    ----------------------------------------------------------------------
                    when commandStateAddress =>
                        -- save address
                        if (iReceiveControlFlag = '0') then
                            if (iMemoryAddressField = "00") then
                                iRMAPAddress (31 downto 24) <= iReceiveData;
                                iMemoryAddressField         <= "01";
                            elsif (iMemoryAddressField = "01") then
                                iRMAPAddress (23 downto 16) <= iReceiveData;
                                iMemoryAddressField         <= "10";
                            elsif (iMemoryAddressField = "10") then
                                iRMAPAddress (15 downto 8) <= iReceiveData;
                                iMemoryAddressField        <= "11";
                            else
                                iRMAPAddress (7 downto 0) <= iReceiveData;
                                iMemoryAddressField       <= "00";
                                commandState              <= commandStateDataLength;
                            end if;
                        else
                            -- Incomplete packet (too short to interpret)
                            iMemoryAddressField <= "00";
                            if (iReceiveData(0) = '0') then  -- Early EOP                              
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.11 Data Length field
                    -- Store the received data into the RMAP Data Length register.
                    ----------------------------------------------------------------------
                    when commandStateDataLength =>
                        -- save data length
                        if (iReceiveControlFlag = '0') then
                            if (iDataLengthField = "00") then
                                iRMAPDataLength (23 downto 16) <= iReceiveData;
                                iDataLengthField               <= "01";
                            elsif (iDataLengthField = "01") then
                                iRMAPDataLength (15 downto 8) <= iReceiveData;
                                iDataLengthField              <= "10";
                            else
                                iRMAPDataLength (7 downto 0) <= iReceiveData;
                                iDataLengthField             <= "00";
                                iCommandCheck                <= '1';

                                commandState <= commandStateHeaderCRC;
                            end if;
                        else
                            -- Incomplete packet (too short to interpret)
                            iDataLengthField <= "00";
                            if (iReceiveData(0) = '0') then  -- Early EOP                              
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.12 Header CRC field
                    -- Compare received data and calculated CRC in a module.
                    -- Then check the data length as supported by RMAP Target IP Core, if CRC
                    -- is correct value.
                    -- If it is supported by RMAP Target IP Core, send the access permission
                    -- to the internal bus, to the user module.
                    ----------------------------------------------------------------------
                    when commandStateHeaderCRC =>
                        if (iReceiveControlFlag = '0') then
                            if (iCommandCRC = iReceiveData) then
                                iDataCount <= (0 => '1', others => '0');

                                if (iRMAPInvalidCommand = '1') then  --Invalid_Command
                                    iErrorCode   <= x"02";
                                    commandState <= commandStateWaitEOP;
                                    iErrorReply  <= '1';

                                elsif (iCommandCheckNG = '1' and iReadModifyWriteLengthNG = '0') then
                                    --Data Lenfth is not supported.
                                    iErrorCode   <= x"0A";
                                    commandState <= commandStateWaitEOP;
                                    iErrorReply  <= '1';

                                elsif (rejectIn = '1') then  --Internal Bus Access denied by user
                                    iErrorCode   <= replyStatusIn;  --ReplyStatus from UserModule
                                    commandState <= commandStateWaitEOP;
                                    iErrorReply  <= '1';

                                elsif (iReadModifyWriteLengthNG = '1') then  --Data Lenfth is not supported.
                                    iErrorCode   <= x"0B";
                                    commandState <= commandStateWaitEOP;
                                    iErrorReply  <= '1';
                                    
                                elsif (authorizeIn = '1') then  --Internal Bus Access allowed by the user
                                    
                                    iAfterHeaderCRC <= '1';
                                    
                                    if (iRMAPCommand (3 downto 1) = "001") then  --Read_Command 
                                        commandState <= commandStateDataEOP;
                                    end if;

                                    if (iRMAPCommand (3 downto 2) = "10") then  --Write_Command  No_Verify
                                        iBusAccessStart   <= '1';
                                        iBusAccessStarted <= '1';
                                        commandState      <= commandStateData;
                                    end if;

                                    if (iRMAPCommand (3 downto 2) = "11") then  --Write_Command  Use_Verify
                                        commandState <= commandStateData;
                                    end if;

                                    if (iRMAPCommand (3 downto 0) = "0111") then  --RMW_Command  Use_Verify
                                        commandState <= commandStateData;
                                    end if;
                                end if;
                                
                            else  -- ECSS-E-ST-50-11C Header CRC error 6.4.3.4.5/6.5.3.4.5/6.6.3.4.5 
                                -- no action, no reply 
                                iErrorCode   <= x"01";
                                commandState <= commandStateWaitEOP;
                            end if;
                        else
                            -- Incomplete packet (too short to interpret)
                            if (iReceiveData(0) = '0') then  -- Early EOP
                                iErrorCode <= x"05";
                            else        -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            commandState <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.13 Data field
                    -- Receive the data of the Write command or ReadModifyWrite command
                    -- and then store it in writeBuffer.
                    ----------------------------------------------------------------------
                    when commandStateData =>

                        iAfterHeaderCRC <= '0';

                        -- if read-modefy-write ... else write ...
                        if (iRMAPCommand (3 downto 2) = "01") then
                            -- read-modefy-write command
                            if (iReceiveControlFlag = '0') then
                                if (iErrorCode /= x"00") then
                                    iErrorReply  <= '1';
                                    commandState <= commandStateWaitEOP;
                                else
                                    if (iDataCount < iRMAPDataLength) then
                                        iDataCount              <= iDataCount + 1;
                                        iWriteBufferWriteEnable <= '1';
                                    else
                                        iDataCount              <= (others => '0');
                                        iWriteBufferWriteEnable <= '1';
                                        commandState            <= commandStateDataCRC;
                                    end if;
                                end if;
                            else
                                if (iReceiveData(0) = '0') then   -- Early EOP
                                    iErrorCode <= x"05";
                                else    -- Receive EEP
                                    iErrorCode <= x"07";
                                end if;
                                iBusAccessStop    <= '1';
                                iBusAccessStarted <= '0';
                                iWriteBufferClear <= '1';
                                commandState      <= commandStateIdle;
                                ----------------------------------------------------------------------
                                --  ECSS-E-ST-50-11C 6.6.3.4.3 Error End of Packet
                                --If an EEP is received immediately after the complete header 
                                --including header CRC has been received the target shall:Not send a reply packet
                                ----------------------------------------------------------------------
                                if (iAfterHeaderCRC = '0') then 
                                    iErrorReply <= '1';
                                end if;

                            end if;
                        else
                            -- write command
                            if (iBusWriteMemoryError = '1') then  -- BusWriteError of No_Verify Write Command
                                iErrorCode        <= x"01";
                                iBusAccessStop    <= '1';
                                iBusAccessStarted <= '0';
                                iWriteBufferClear <= '1';
                                iErrorReply       <= '1';
                                commandState      <= commandStateWaitEOP;
                            elsif (iReceiveControlFlag = '0') then
                                if (iErrorCode /= x"00") then
                                    iErrorReply  <= '1';
                                    commandState <= commandStateWaitEOP;
                                elsif (iRMAPDataLength > 2048 and iRMAPCommand(2) = '1') then
                                    iErrorCode   <= x"09";
                                    iErrorReply  <= '1';
                                    commandState <= commandStateWaitEOP;
                                elsif (writeBufferFull = '0') then
                                    if (iDataCount < iRMAPDataLength) then
                                        iDataCount              <= iDataCount + 1;
                                        iWriteBufferWriteEnable <= '1';
                                    else
                                        iDataCount              <= (others => '0');
                                        iWriteBufferWriteEnable <= '1';
                                        commandState            <= commandStateDataCRC;
                                    end if;
                                end if;
                            else
                                if (iReceiveData(0) = '0') then   -- Early EOP
                                    iErrorCode <= x"05";
                                else    -- Receive EEP
                                    iErrorCode <= x"07";
                                end if;
                                iBusAccessStop    <= '1';
                                iBusAccessStarted <= '0';
                                iWriteBufferClear <= '1';
                                commandState      <= commandStateIdle;
                                ----------------------------------------------------------------------
                                --  ECSS-E-ST-50-11C 6.4.3.4.3 Error End of Packet
                                --If an EEP is received immediately after the complete header 
                                --including header CRC has been received the target shall:Not send a reply packet
                                ----------------------------------------------------------------------
                                if (iAfterHeaderCRC = '0') then 
                                    iErrorReply <= '1';
                                end if;
                            end if;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.2.15 Data CRC field
                    -- Compare received data and calculated CRC in a module.
                    ----------------------------------------------------------------------
                    when commandStateDataCRC =>
                        if (iReceiveControlFlag = '0') then
                            if (iCommandCRC = iReceiveData) then
                                commandState <= commandStateDataEOP;
                            else
                                -- data crc error
                                iErrorCode        <= x"04";
                                iBusAccessStop    <= '1';
                                iBusAccessStarted <= '0';
                                iWriteBufferClear <= '1';
                                commandState      <= commandStateDataEOP;
                            end if;
                        else
                            if (iReceiveData(0) = '0') then  -- Early EOP
                                iErrorCode <= x"05";
                            else                             -- Receive EEP
                                iErrorCode <= x"07";
                            end if;
                            iBusAccessStop    <= '1';
                            iBusAccessStarted <= '0';
                            iWriteBufferClear <= '1';
                            iErrorReply       <= '1';
                            commandState      <= commandStateIdle;
                        end if;

                    ----------------------------------------------------------------------
                    --  ECSS-E-ST-50-11C 6.4.1.16 EOP character
                    --  ECSS-E-ST-50-11C 6.5.1.14 EOP character
                    --  ECSS-E-ST-50-11C 6.6.2.14 EOP character
                    -- Check the packet terminated by EOP.
                    -- If received data is not EOP, commandState moves to commandStateWaitEOP,
                    -- then wait until receiving EOP.
                    ----------------------------------------------------------------------
                    when commandStateDataEOP =>
                        if (iReceiveControlFlag = '0') then
                            -- Too much data Error
                            iErrorCode   <= x"06";
                            iErrorReply  <= '1';
                            commandState <= commandStateWaitEOP;
                        else
                            if (iErrorCode /= x"00") then  --4(data crc error) or 2(unused RMAP Packet Type or Command Code)
                                iErrorReply <= '1';
                                if (iRMAPCommand (3 downto 2) = "01" or iRMAPCommand (3) = '1') then
                                    --RMW_Command or Write_Command
                                    iBusAccessStop    <= '1';
                                    iBusAccessStarted <= '0';
                                    iWriteBufferClear <= '1';
                                end if;
                                
                            elsif (iReceiveData(0) = '0') then  -- Receive EOP
                                if (iRMAPCommand (3 downto 2) = "11" or iRMAPCommand (3) = '0') then
                                    --Write_Command With_Verify or Read_Command
                                    iBusAccessStart   <= '1';
                                    iBusAccessStarted <= '1';
                                end if;
                                
                            else        -- Receive EEP
                                iErrorCode        <= x"07";
                                iBusAccessStop    <= '1';
                                iBusAccessStarted <= '0';
                                iWriteBufferClear <= '1';
                                ----------------------------------------------------------------------
                                --  ECSS-E-ST-50-11C 6.4.3.4.3 Error End of Packet
                                --If an EEP is received immediately after the complete header 
                                --including header CRC has been received the target shall:Not send a reply packet
                                ----------------------------------------------------------------------
                                if (iAfterHeaderCRC = '0') then 
                                    iErrorReply <= '1';
                                end if;
                            end if;
                            commandState <= commandStateWaitBusAccessEnd;
                        end if;

                    ----------------------------------------------------------------------
                    -- If there is an error in the RMAP Packet. The commandState will move to 
                    -- this state and discard data until receiving the EEP or EOP.
                    ----------------------------------------------------------------------
                    when commandStateWaitEOP =>
                        if (receiveFIFODataOut (8) = '1') then
                            if (receiveFIFODataOut (0) = '0') then
                                -- EOP
                                else
                                    -- EEP
                            end if;
                            commandState <= commandStateIdle;
                        else
                            -- wait until EOP or EEP.
                        end if;
                        
                    when others => null;
                end case;
                
            elsif (busMasterTimeOutErrorIn /= '1') then
                iWriteBufferWriteEnable <= '0';
                iBusAccessStop          <= '0';
                iWriteBufferClear       <= '0';
                iBusAccessStart         <= '0';
                iErrorReply             <= '0';
                
            end if;
        end if;
    end process;


--------------------------------------------------------------------------------
-- receive data crc table (0-255:RevE, 256-511:RevF)
--------------------------------------------------------------------------------
    iCommandCRCRomAddress <= cRMAPCRCRevision & iCommandCRCRomAddressBuffer;

----------------------------------------------------------------------
-- Xilinx
----------------------------------------------------------------------
    receiveCRCRomXilinx : if cUseDevice = 1 generate
        receiveCRCRom : RMAPTargetIPCRCRomXilinx
            port map (
                clock   => clock,
                address => iCommandCRCRomAddress,
                dataOut => commandCRCRomOut
                );
    end generate;

----------------------------------------------------------------------
-- Altera
----------------------------------------------------------------------
    receiveCRCRomAltera : if cUseDevice = 0 generate
        receiveCRCRom : RMAPTargetIPCRCRomAltera
            port map (
                clock   => clock,
                address => iCommandCRCRomAddress,
                dataOut => commandCRCRomOut
                );
    end generate;

--------------------------------------------------------------------------------


    ----------------------------------------------------------------------
    --  ECSS-E-ST-50-11C 6.3 Cyclic Redundancy Code
    --  ECSS-E-ST-50-11C 6.10.3 C-code implementation of RMAP CRC
    -- Read the Receive data from the receiveFIFO and calculate CRC from Receive data.
    ----------------------------------------------------------------------
    process (clock, reset)
    begin
        if (reset = '1') then
            receiveFIFOState            <= receiveFIFOStateIdle;
            iReceiveFIFOReadEnable      <= '0';
            iCommandCRC                 <= (others => '0');
            iCommandCRCRomAddressBuffer <= (others => '0');
            iReceiveFIFODataReady       <= '0';
            iReceiveControlFlag         <= '0';
            iReceiveData                <= x"00";
            
        elsif (clock'event and clock = '1') then

            case receiveFIFOState is

                ----------------------------------------------------------------------
                -- Read the Receive data from receiveFIFO when the data in the receiveFIFO.
                ----------------------------------------------------------------------
                when receiveFIFOStateIdle =>
                    if ((iReplyProcessing = '0' and not(commandState = commandStateWaitBusAccessEnd)) or (commandState = commandStateWaitEOP)) then
                        --Not ReplyProcessing and Not BusAccessProcessing
                        if (iReceiveFIFOReady = '1') then  --spaceWireCODECIP reeceiveFIFO is not Empty
                            iReceiveFIFOReadEnable <= '1';  --Read receiveFIFO
                            receiveFIFOState       <= receiveFIFOStateRead0;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                -- Wait to Read the Receive data from receiveFIFO.
                ----------------------------------------------------------------------
                when receiveFIFOStateRead0 =>
                    iReceiveFIFOReadEnable <= '0';
                    if (commandState = commandStateHeaderCRC) then
                        if (authorizeIn = '1' or rejectIn = '1' or iCommandCheckNG = '1') then
                            receiveFIFOState <= receiveFIFOStateRead1;
                        end if;
                    else
                        receiveFIFOState <= receiveFIFOStateRead1;
                    end if;

                ----------------------------------------------------------------------
                -- Complete Receive data read. 
                -- Output the ready signal(iReceiveFIFODataReady).
                ----------------------------------------------------------------------  
                when receiveFIFOStateRead1 =>
                    iReceiveControlFlag   <= receiveFIFODataOut (8);
                    iReceiveData          <= receiveFIFODataOut (7 downto 0);
                    iReceiveFIFODataReady <= '1';
                    receiveFIFOState      <= receiveFIFOStateRead2;

                ----------------------------------------------------------------------
                -- If Receive data is DataCharacter, generate ROM Address for calculate CRC.
                -- If Receive data is ControlCharacter, set ROM address for the CRC calculation to x"00".
                ----------------------------------------------------------------------  
                when receiveFIFOStateRead2 =>
                    iReceiveFIFODataReady <= '0';
                    if (receiveFIFODataOut (8) = '1') then
                        iCommandCRCRomAddressBuffer <= x"00";
                    else
                        iCommandCRCRomAddressBuffer <= iCommandCRC xor receiveFIFODataOut (7 downto 0);
                    end if;
                    receiveFIFOState <= receiveFIFOStateRead3;

                ----------------------------------------------------------------------
                -- Wait to Read the CRCdata from CRCRom.
                ----------------------------------------------------------------------  
                when receiveFIFOStateRead3 =>
                    receiveFIFOState <= receiveFIFOStateRead4;

                ----------------------------------------------------------------------
                -- Adopt the CRC data read from CRCRom as the current CRC value.
                ----------------------------------------------------------------------  
                when receiveFIFOStateRead4 =>
                    iCommandCRC      <= commandCRCRomOut;
                    receiveFIFOState <= receiveFIFOStateIdle;
                    
                when others => null;

            end case;
        end if;
    end process;


--------------------------------------------------------------------------------
-- reply data crc table (0-255:RevE, 256-511:RevF)
--------------------------------------------------------------------------------
    iReplyCRCRomAddress <= cRMAPCRCRevision & iReplyCRCAddressBuffer;

----------------------------------------------------------------------
-- Xilinx
----------------------------------------------------------------------
    transmittCRCRomXilinx : if cUseDevice = 1 generate
        transmitteCRCRom : RMAPTargetIPCRCRomXilinx
            port map (
                clock   => clock,
                address => iReplyCRCRomAddress,
                dataOut => replyCRCRomOut
                );
    end generate;

----------------------------------------------------------------------
-- Altera
----------------------------------------------------------------------
    transmittCRCRomAltera : if cUseDevice = 0 generate
        transmitteCRCRom : RMAPTargetIPCRCRomAltera
            port map (
                clock   => clock,
                address => iReplyCRCRomAddress,
                dataOut => replyCRCRomOut
                );
    end generate;

--------------------------------------------------------------------------------


    ----------------------------------------------------------------------
    --  ECSS-E-ST-50-11C 6.3 Cyclic Redundancy Code
    --  ECSS-E-ST-50-11C 6.10.3 C-code implementation of RMAP CRC--
    --
    -- Calculate the CRC data of ReplyPacket and 
    -- store ReplyPacket into the transmitFIFO.
    ----------------------------------------------------------------------
    process (clock, reset)
    begin

        if (reset = '1') then
            transmitFIFOState        <= transmitFIFOStateIdle;
            iTransmitFIFOWriteEnable <= '0';
            iReplyCRCOut             <= (others => '0');
            iReplyCRCAddressBuffer   <= (others => '0');
            iCRCByteCalculated       <= '0';
            
        elsif (clock'event and clock = '1') then

            case transmitFIFOState is

                ----------------------------------------------------------------------
                -- If Transmit data is DataCharacter, generate ROM Address for calculate CRC.
                -- If Transmit data is ControlCharacter, set ROM address for the CRC calculation to x"00".
                ----------------------------------------------------------------------  
                when transmitFIFOStateIdle =>
                    iTransmitFIFOWriteEnable <= '0';
                    iCRCByteCalculated       <= '0';
                    if (iReplyDataSet = '1') then
                        if (iTransmitFIFODataIn (8) = '0') then
                            iReplyCRCAddressBuffer <= iReplyCRCOut xor iTransmitFIFODataIn (7 downto 0);
                        else
                            iReplyCRCAddressBuffer <= x"00";
                        end if;
                        transmitFIFOState <= transmitFIFOStateWrite0;
                    elsif (iReplySpaceWireAddressSet = '1') then
                        transmitFIFOState <= transmitFIFOStateWrite2;
                    end if;

                ----------------------------------------------------------------------
                -- WaitTime to Read the CRCdata from CRCRom.
                ----------------------------------------------------------------------  
                when transmitFIFOStateWrite0 =>
                    transmitFIFOState <= transmitFIFOStateWrite1;

                ----------------------------------------------------------------------
                -- Adopt the CRC data read from CRCRom as the current CRC value.
                ----------------------------------------------------------------------  
                when transmitFIFOStateWrite1 =>
                    if (iTransmitFIFOReady = '1') then
                        iTransmitFIFOWriteEnable <= '1';
                        iReplyCRCOut             <= replyCRCRomOut;
                        iCRCByteCalculated       <= '1';
                        transmitFIFOState        <= transmitFIFOStateIdle;
                    end if;

                ----------------------------------------------------------------------
                -- If transmitFIFO is not full, write 1 character ReplyPacket to transmitFIFO.
                ----------------------------------------------------------------------  
                when transmitFIFOStateWrite2 =>
                    if (iTransmitFIFOReady = '1') then
                        iTransmitFIFOWriteEnable <= '1';
                        iCRCByteCalculated       <= '1';
                        transmitFIFOState        <= transmitFIFOStateIdle;
                    end if;
                    
                when others => null;

            end case;
        end if;
    end process;


----------------------------------------------------------------------
-- ECSS-E-ST-50-11C 6.4.2 Write reply format
-- ECSS-E-ST-50-11C 6.5.2 Read reply format
-- ECSS-E-ST-50-11C 6.6.2 Read-modify-write reply format
-- ECSS-E-ST-50-11C 6.7 Error and status codes
---------------------------------------------------------------------- 
    process (clock, reset)
    begin
        if (reset = '1') then
            replyState                 <= replyStateIdle;
            iCRCCalculatePhase         <= '0';
            iReplyProcessing           <= '0';
            iTransmitFIFODataIn        <= (others => '0');
            iReplyDataSet              <= '0';
            iReplyTransactionIDField   <= '0';
            iReplyDataLengthField      <= "00";
            iReadBufferReadEnable      <= '0';
            iReplyDataLength           <= (others => '0');
            iReplyDataCount            <= (others => '0');
            iReplyAddressFieldTransmit <= 0;
            iReplySpaceWireAddressSet  <= '0';
            iBusReadMemoryError        <= '0';
            iReadBufferClear           <= '0';
            
        elsif (clock'event and clock = '1') then

            ----------------------------------------------------------------------
            --  ECSS-E-ST-50-11C 6.5.3.10 Read not OK
            -- If memory fails in the read memory operation, the target should stop
            -- reading the data from memory as soon as the memory error is detected.
            --
            -- If memory fails in the read memory operation, the target shall Aappend
            -- an EEP to the end of the data already sent in the reply to the initiator.
            ----------------------------------------------------------------------
            if (busMasterTimeOutErrorIn = '1') then
                iBusReadMemoryError <= '1';
            elsif (replyState = replyStateIdle) then
                iBusReadMemoryError <= '0';
            end if;

            case replyState is
                ----------------------------------------------------------------------
                -- State which wait to start the Reply operation.
                ----------------------------------------------------------------------
                when replyStateIdle =>
                    if (iBusAccessStart = '1' and iRMAPCommand2 (3) = '0') then  --Read command Reply Start
                        iReplyProcessing <= '1';
                        if (iReplyAddressFieldReceive /= 0) then
                            iReplyAddressFieldTransmit <= 0;
                            replyState                 <= replyStatePathAddress;
                        else
                            replyState <= replyStateInitiatorLogicalAddress;
                        end if;
                        
                    elsif (iReplyStart = '1' and iRMAPCommand2 (3) = '1' and iRMAPCommand2 (1) = '1') then  --Write command Reply Start
                        iReplyProcessing <= '1';
                        if (iReplyAddressFieldReceive /= 0) then
                            iReplyAddressFieldTransmit <= 0;
                            replyState                 <= replyStatePathAddress;
                        else
                            replyState <= replyStateInitiatorLogicalAddress;
                        end if;

                    elsif (iErrorReply = '1' and iRMAPCommand2 (1) = '1') then  --Error Reply Start
                        iReplyProcessing <= '1';
                        -- Error Code != 0
                        if (iReplyAddressFieldReceive /= 0) then
                            iReplyAddressFieldTransmit <= 0;
                            replyState                 <= replyStatePathAddress;
                        else
                            replyState <= replyStateInitiatorLogicalAddress;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                -- ECSS-E-ST-50-11C 6.2.16 Reply SpaceWire Address field
                -- Store the SpaceWireAddress data into transmitFIFO.
                -- StateMachine move to replyStateInitiatorLogicalAddress,
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStatePathAddress =>
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn (8)          <= '0';
                        iTransmitFIFODataIn (7 downto 0) <= iReplyAddress2(iReplyAddressFieldTransmit);
                        iReplyAddressFieldTransmit       <= iReplyAddressFieldTransmit + 1;
                        iReplySpaceWireAddressSet        <= '1';
                        iCRCCalculatePhase               <= '1';
                    else
                        iReplySpaceWireAddressSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            if (iReplyAddressFieldTransmit = iReplyAddressFieldReceive) then
                                replyState <= replyStateInitiatorLogicalAddress;
                            end if;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                -- ECSS-E-ST-50-11C 6.2.7 Initiator Logical Address field
                -- Store the InitiatorLogicalAddress data into transmitFIFO.
                -- StateMachine move to replyStateProtocolID,
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateInitiatorLogicalAddress =>
                    -- transmit path address and logical address
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn (8)          <= '0';
                        iTransmitFIFODataIn (7 downto 0) <= iInitiatorLogicalAddress;
                        iReplyDataSet                    <= '1';
                        iCRCCalculatePhase               <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            replyState         <= replyStateProtocolID;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.2.3 Protocol Identifier field
                -- Store the RMAPProtocolID data(0x01) into transmitFIFO.
                -- StateMachine moves to replyStateInstruction,
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateProtocolID =>
                    -- transmit protocol id(0x01=RMAP)
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn (8)          <= '0';
                        iTransmitFIFODataIn (7 downto 0) <= cRMAPProtocolID;
                        iReplyDataSet                    <= '1';
                        iCRCCalculatePhase               <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            replyState         <= replyStateInstruction;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.2.4 Instruction field
                -- Store the RMAPCommand data and ReplyAddressSize data into transmitFIFO.
                -- StateMachine move to replyStateStatus,
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateInstruction =>
                    -- transmit reply command
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn (8)          <= '0';
                        iTransmitFIFODataIn (7 downto 6) <= "00";  --packet type Reply
                        iTransmitFIFODataIn (5 downto 2) <= iRMAPCommand2;
                        iTransmitFIFODataIn (1 downto 0) <= iReplyAddressSize2;
                        iReplyDataSet                    <= '1';
                        iCRCCalculatePhase               <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            replyState         <= replyStateStatus;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.2.17 Status field
                --  ECSS-E-ST-50-11C 6.4.2.6/6.5.2.7/6.6.2.7  Status field                
                -- Store the 0x00 in transmitFIFO if the command executed successfully.
                -- Store the ErrorCode in transmitFIFO if the command executed illegally.
                ----------------------------------------------------------------------
                when replyStateStatus =>
                    -- transmit status code
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn (8)          <= '0';
                        iTransmitFIFODataIn (7 downto 0) <= iErrorCode;
                        iReplyDataSet                    <= '1';
                        iCRCCalculatePhase               <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            replyState         <= replyStateTargetLogicalAddress;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.4.2.7/6.5.2.8/6.6.2.8  Target Logical Address field
                -- Store the TargetLogicalAddress data in transmitFIFO.
                -- StateMachine moves to replyStateStatus,
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateTargetLogicalAddress =>
                    -- transmit my addredd
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn (8)          <= '0';
                        iTransmitFIFODataIn (7 downto 0) <= iTargetLogicalAddress;
                        iReplyDataSet                    <= '1';
                        iCRCCalculatePhase               <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            replyState         <= replyStateTransactionID;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.4.2.8/6.5.2.9/6.6.2.9  Transaction Identifier field
                -- Store the TransactionID data into transmitFIFO.
                -- Write command reply
                --   StateMachine move to replyStateHeaderCRC,
                --   after complete the calculation of the Transmit CRC.
                -- Read command reply or RMW command reply
                --   StateMachine move to iRMAPDataLength,
                --   after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateTransactionID =>
                    -- transmit received transaction id
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn (8) <= '0';
                        if (iReplyTransactionIDField = '0') then
                            iTransmitFIFODataIn (7 downto 0) <= iTransactionID (15 downto 8);
                        else
                            iTransmitFIFODataIn (7 downto 0) <= iTransactionID (7 downto 0);
                        end if;
                        iReplyDataSet      <= '1';
                        iCRCCalculatePhase <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            if (iReplyTransactionIDField = '0') then
                                iReplyTransactionIDField <= '1';
                            else
                                iReplyTransactionIDField <= '0';
                                if (iRMAPCommand2 (3 downto 2) = "00") then
                                    if (iErrorCode /= x"00") then
                                        iReplyDataLength <= (others => '0');
                                    else
                                        iReplyDataLength <= iRMAPDataLength;
                                    end if;
                                    replyState <= replyStateReservedByte;
                                elsif (iRMAPCommand2 (3 downto 2) = "01") then
                                    if (iErrorCode /= x"00") then
                                        iReplyDataLength <= (others => '0');
                                    else
                                        iReplyDataLength <= '0' & iRMAPDataLength (23 downto 1);
                                    end if;
                                    replyState <= replyStateReservedByte;
                                else
                                    iReplyDataLength <= iRMAPDataLength;
                                    replyState       <= replyStateHeaderCRC;
                                end if;
                            end if;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                -- Store the 0x00 in transmitFIFO.
                -- StateMachine move to replyStateDataLength,
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateReservedByte =>
                    -- reserved byte (= 0x00)
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn <= '0' & x"00";
                        iReplyDataSet       <= '1';
                        iCRCCalculatePhase  <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            replyState         <= replyStateDataLength;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.2.11 Data Length field
                -- Store the ReplyDataLength data into transmitFIFO.
                -- StateMachine moves to replyStateHeaderCRC,
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateDataLength =>
                    -- transmit data length
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn (8) <= '0';
                        if (iReplyDataLengthField = "00") then
                            iTransmitFIFODataIn (7 downto 0) <= iReplyDataLength (23 downto 16);
                        elsif (iReplyDataLengthField = "01") then
                            iTransmitFIFODataIn (7 downto 0) <= iReplyDataLength (15 downto 8);
                        else
                            iTransmitFIFODataIn (7 downto 0) <= iReplyDataLength (7 downto 0);
                        end if;
                        iReplyDataSet      <= '1';
                        iCRCCalculatePhase <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            if (iReplyDataLengthField = "00") then
                                iReplyDataLengthField <= "01";
                            elsif (iReplyDataLengthField = "01") then
                                iReplyDataLengthField <= "10";
                            else
                                iReplyDataLengthField <= "00";
                                replyState            <= replyStateHeaderCRC;
                            end if;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.2.12 Header CRC field
                -- Store CRC data into transmitFIFO.
                -- StateMachine move to replyStateData,
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateHeaderCRC =>
                    if (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn <= '0' & iReplyCRCOut;
                        iReplyDataSet       <= '1';
                        iCRCCalculatePhase  <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';

                            if (iRMAPCommand2 (3) = '1') then  -- Write Command Reply
                                replyState <= replyStateEOP;
                            else        -- Read Command Reply
                                iReplyDataCount <= (others => '0');
                                replyState      <= replyStateDataWait;
                            end if;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.2.13 Data field
                -- Read the data from readBuffer, when the data is in the readBuffer.
                ----------------------------------------------------------------------
                when replyStateDataWait =>
                    if (iBusReadMemoryError = '1') then
                        iReadBufferClear   <= '1';
                        iCRCCalculatePhase <= '0';
                        replyState         <= replyStateEOP;
                        
                    elsif (iErrorCode /= x"00") then
                        replyState <= replyStateDataCRC;
                    else
                        if (iReadBufferReady = '1') then
                            iReadBufferReadEnable <= '1';
                        end if;

                        if (iReadBufferReadEnable = '1') then
                            iReadBufferReadEnable <= '0';
                            replyState            <= replyStateData;
                        end if;
                        
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.2.13 Data field
                -- Store the data(read from the readBuffer) into transmitFIFO.
                -- StateMachine moves to replyStateDataWait or replyStateDataCRC,
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateData =>
                    if (iBusReadMemoryError = '1') then
                        iReadBufferClear   <= '1';
                        iCRCCalculatePhase <= '0';
                        replyState         <= replyStateEOP;
                        
                    elsif (iCRCCalculatePhase = '0') then
                        iTransmitFIFODataIn (8) <= '0';
                        if (iReplyDataCount < iReplyDataLength) then
                            iTransmitFIFODataIn (7 downto 0) <= readBufferReadData;
                            iReplyDataCount                  <= iReplyDataCount + 1;
                        end if;
                        iReplyDataSet      <= '1';
                        iCRCCalculatePhase <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            if (iReplyDataCount < iReplyDataLength) then
                                replyState <= replyStateDataWait;
                            else
                                replyState <= replyStateDataCRC;
                            end if;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.2.15 Data CRC field
                -- Store CRC data in transmitFIFO.
                -- after complete the calculation of the Transmit CRC.
                -- StateMachine moves to replyStateEOP. 
                ----------------------------------------------------------------------
                when replyStateDataCRC =>
                    if (iCRCCalculatePhase = '0') then
                        if (iErrorCode /= x"00") then
                            iTransmitFIFODataIn <= '0' & x"00";
                        else
                            iTransmitFIFODataIn <= '0' & iReplyCRCOut;
                        end if;
                        iReplyDataSet       <= '1';
                        iCRCCalculatePhase  <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            replyState         <= replyStateEOP;
                        end if;
                    end if;

                ----------------------------------------------------------------------
                --  ECSS-E-ST-50-11C 6.4.2.10/6.5.2.14/6.6.2.14 EOP character
                -- Store the EOP data into transmitFIFO.
                -- StateMachine move to replyStateIdle, 
                -- after complete the calculation of the Transmit CRC.
                ----------------------------------------------------------------------
                when replyStateEOP =>
                    iBusReadMemoryError <= '0';
                    iReadBufferClear    <= '0';

                    if (iCRCCalculatePhase = '0') then
                        if (iBusReadMemoryError = '1') then
                            iTransmitFIFODataIn <= '1' & x"01";  --EEP
                        else
                            iTransmitFIFODataIn <= '1' & x"00";  --EOP
                        end if;
                        iReplyDataSet      <= '1';
                        iCRCCalculatePhase <= '1';
                    else
                        iReplyDataSet <= '0';
                        if (iCRCByteCalculated = '1') then
                            iCRCCalculatePhase <= '0';
                            replyState         <= replyStateIdle;
                            iReplyProcessing   <= '0';
                        end if;
                    end if;
                    
                when others => null;
            end case;
        end if;
    end process;


--------------------------------------------------------------------------------
-- read buffer  (written by dma, read by rmap)
--
-- write buffer (written by rmap, read by dma)
--------------------------------------------------------------------------------
    iDMAReadBufferWriteData <= dmaReadBufferWriteData;
    iWriteBufferReset       <= iWriteBufferClear or reset;
    iReadBufferReset        <= iReadBufferClear or reset;
    ----------------------------------------------------------------------
    -- Xilinx
    ----------------------------------------------------------------------
    XilinxFIFO : if cUseDevice = 1 generate
        readBuffer : RMAPTargetIPBufferFIFO8x2kXilinx
            port map (
                clock       => clock,
                reset       => iReadBufferReset,
                writeEnable => iDMAReadBufferWriteEnable,
                dataIn      => iDMAReadBufferWriteData,
                readEnable  => iReadBufferReadEnable,
                dataOut     => readBufferReadData,
                full        => readBufferFull,
                empty       => readBufferEmpty,
                dataCount   => readBufferDataCount  --debug
                );

        writeBuffer : RMAPTargetIPBufferFIFO8x2kXilinx
            port map (
                clock       => clock,
                reset       => iWriteBufferReset,
                writeEnable => iWriteBufferWriteEnable,
                dataIn      => iReceiveData,
                readEnable  => dmaWriteBufferReadEnable,
                dataOut     => dmaWriteBufferReadData,
                full        => writeBufferFull,
                empty       => dmaWriteBufferEmpty,
                dataCount   => writeBufferDataCount  --debug
                );
    end generate;

    ----------------------------------------------------------------------
    -- Altera
    ----------------------------------------------------------------------
    AlteraFIFO : if cUseDevice = 0 generate
        readBuffer : RMAPTargetIPBufferFIFO8x2kAltera
            port map (
                clock       => clock,
                reset       => iReadBufferReset,
                writeEnable => iDMAReadBufferWriteEnable,
                dataIn      => iDMAReadBufferWriteData,
                readEnable  => iReadBufferReadEnable,
                dataOut     => readBufferReadData,
                full        => readBufferFull,
                empty       => readBufferEmpty,
                dataCount   => readBufferDataCount  --debug
                );

        writeBuffer : RMAPTargetIPBufferFIFO8x2kAltera
            port map (
                clock       => clock,
                reset       => iWriteBufferReset,
                writeEnable => iWriteBufferWriteEnable,
                dataIn      => iReceiveData,
                readEnable  => dmaWriteBufferReadEnable,
                dataOut     => dmaWriteBufferReadData,
                full        => writeBufferFull,
                empty       => dmaWriteBufferEmpty,
                dataCount   => writeBufferDataCount  --debug
                );
    end generate;

end behavioral;
