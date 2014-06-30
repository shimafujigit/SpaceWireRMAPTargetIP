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
use work.RMAPTargetIPPackage.all;

entity RMAPTargetIPDMAController is
    generic (
        gBusWidth : integer range 8 to 32 := 32);  -- 8 = 8bit, 16 = 16bit, 32 = 32bit,
    port (
        clock                    : in  std_logic;
        reset                    : in  std_logic;
        --RMAP <-> DMA
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
        -- DMA <-> USER
        busMasterCycleOut        : out std_logic;
        busMasterStrobeOut       : out std_logic;
        busMasterAddressOut      : out std_logic_vector (31 downto 0);
        busMasterByteEnableOut   : out std_logic_vector (3 downto 0);
        busMasterDataIn          : in  std_logic_vector (31 downto 0);
        busMasterDataOut         : out std_logic_vector (31 downto 0);
        busMasterWriteEnableOut  : out std_logic;
        busMasterReadEnableOut   : out std_logic;
        busMasterAcknowledgeIn   : in  std_logic;
        busMasterTimeOutErrorIn  : in  std_logic
        );
end RMAPTargetIPDMAController;

architecture behavioral of RMAPTargetIPDMAController is
    
    type dmaStateMachine is (
        dmaStateIdle,
        dmaStateWrite0,
        dmaStateWrite1,
        dmaStateWrite2,
        dmaStateWrite3,
        dmaStateWrite4,
        dmaStateWrite5,
        dmaStateRead0,
        dmaStateRead1,
        dmaStateRead2,
        dmaStateRead3,
        dmaStateRead4,
        dmaStateReadModifyWriteControl0,
        dmaStateReadModifyWriteControl1,
        dmaStateReadModifyWriteControl2,
        dmaStateReadModifyWriteWrite0,
        dmaStateReadModifyWriteWrite1,
        dmaStateReadModifyWriteWrite2,
        dmaStateReadModifyWriteWrite3
        );
    signal dmaState : dmaStateMachine;

    signal iBusMasterCycleOut            : std_logic;
    signal iBusMasterStrobeOut           : std_logic;
    signal iBusMasterAddressOutBuffer    : std_logic_vector (31 downto 0);
    signal iBusMasterAddressOut          : std_logic_vector (31 downto 0);
    signal iBusMasterByteSelectOut       : std_logic_vector (3 downto 0);
    signal iBusMasterDataOut             : std_logic_vector (31 downto 0);
    signal iBusMasterDataIn              : std_logic_vector (31 downto 0);
    signal iBusMasterWriteEnableOut      : std_logic;
    signal iBusMasterReadEnableOut       : std_logic;
    signal iBusAccessEnd                 : std_logic;
    signal iBusMasterAcknowledgeReceived : std_logic;
    --
    signal iBusReadByteCount             : integer range 0 to 4;
    --
    signal iDataLength                   : integer range 1 to 4;
    signal iRMAPDataLength               : std_logic_vector (23 downto 0);
    signal iDMAWriteBufferReadEnable     : std_logic;
    signal iDMAReadBufferWriteEnable     : std_logic;
    signal iDMAReadBufferWriteByteSelect : std_logic_vector (3 downto 0);
    --
    signal iReadModifyWriteAddressOut    : std_logic_vector (31 downto 0);
    signal iReadModifyWriteBufferCount   : std_logic_vector (3 downto 0);
    signal iReadModifyWriteDataLength    : std_logic_vector (2 downto 0);
    signal iReadModifyWriteByteCount     : integer range 0 to 4;
    signal iReadModifyWriteReadData      : std_logic_vector (31 downto 0);
    signal iReadModifyWriteReadDataCount : integer range 0 to 3;
    signal iReadModifyWriteWriteData     : std_logic_vector (31 downto 0);
    signal iReadModifyWriteMaskData      : std_logic_vector (31 downto 0);
    signal iReadModifyWriteCommand       : std_logic;
    signal iReadModifyWriteDataOut       : std_logic_vector (31 downto 0);
    signal iDMAReadBufferWriteData       : std_logic_vector (7 downto 0);



    
begin
    dmaReadBufferWriteData   <= iDMAReadBufferWriteData;
    dmaWriteBufferReadEnable <= iDMAWriteBufferReadEnable;
    dmaReadBufferWriteEnable <= iDMAReadBufferWriteEnable;
    busAccessEnd             <= iBusAccessEnd;

    idmaReadBufferWriteData <= iBusMasterDataIn (31 downto 24) when iDMAReadBufferWriteByteSelect = "1000" else
                               iBusMasterDataIn (23 downto 16) when iDMAReadBufferWriteByteSelect = "0100" else
                               iBusMasterDataIn (15 downto 8)  when iDMAReadBufferWriteByteSelect = "0010" else
                               iBusMasterDataIn (7 downto 0)   when iDMAReadBufferWriteByteSelect = "0001" else x"00";
    
    process (clock, reset)
    begin
        if (reset = '1') then
            dmaState                      <= dmaStateIdle;
            iBusMasterCycleOut            <= '0';
            iBusMasterStrobeOut           <= '0';
            iBusMasterAddressOutBuffer    <= (others => '0');
            iBusMasterAddressOut          <= (others => '0');
            iReadModifyWriteAddressOut    <= (others => '0');
            iBusMasterByteSelectOut       <= (others => '0');
            iBusMasterDataIn              <= (others => '0');
            iBusMasterDataOut             <= (others => '0');
            iBusMasterWriteEnableOut      <= '0';
            iBusMasterReadEnableOut       <= '0';
            --
            iBusAccessEnd                 <= '0';
            iDMAWriteBufferReadEnable     <= '0';
            iDMAReadBufferWriteEnable     <= '0';
            iDMAReadBufferWriteByteSelect <= "0000";
            --
            iReadModifyWriteCommand       <= '0';
            iReadModifyWriteDataLength    <= "000";
            iReadModifyWriteByteCount     <= 1;
            iReadModifyWriteBufferCount   <= "0000";
            iReadModifyWriteReadData      <= (others => '0');
            iReadModifyWriteWriteData     <= (others => '0');
            iReadModifyWriteMaskData      <= (others => '0');
            iReadModifyWriteDataOut       <= (others => '0');
            --
            iRMAPDataLength               <= (others => '0');
            iBusMasterAcknowledgeReceived <= '0';
            --
            iReadModifyWriteReadDataCount <= 0;
            iBusReadByteCount             <= 0;
            iDataLength                   <= 1;
            
            
        elsif (clock'event and clock = '1') then
            if (busAccessStop = '1' or busMasterTimeOutErrorIn = '1') then
                dmaState <= dmaStateIdle;

                iBusMasterByteSelectOut       <= (others => '0');
                iBusMasterCycleOut            <= '0';
                iBusMasterStrobeOut           <= '0';
                iBusMasterWriteEnableOut      <= '0';
                iBusMasterWriteEnableOut      <= '0';
                iBusMasterReadEnableOut       <= '0';
                iDMAWriteBufferReadEnable     <= '0';
                iDMAReadBufferWriteEnable     <= '0';
                iDMAReadBufferWriteByteSelect <= "0000";
                iReadModifyWriteCommand       <= '0';
            else
                ----------------------------------------------------------------------
                -- DMAController operation start when it receives 
                -- a busAccessStart signal which came from RMAPDecoder.
                --
                ----------------------------------------------------------------------
                -- Write command operation (dmaStateIdle -> dmaStateWrite0 to 5)
                -- 1: Read a byte of data at a time from writeBuffer of RMAPDecodeer to 
                --    store in iBusMasterDataOut
                -- 2: When the data is stored for a internal bus width, it write to memory
                --    via the internal bas.
                --
                -- Read command operation (dmaStateIdle -> dmaStateRead0 to 4)
                -- 1: Read data (size of a bus width) from memory via the internal bus.
                -- 2: Write the Read data to readBuffer of RMAPDecodeer a byte at a time.
                --
                -- ReadModifyWrite command operation 
                --     (dmaStateIdle -> dmaStateReadModifyWriteControl0 to 2  
                --         -> dmaStateRead0 to 3 -> dmaStateReadModifyWriteWrite0 to 3)
                -- 1: Read a byte of the write and mask data at a time from writeBuffer of
                --    RMAPDecodeer, then store to iReadModifyWriteMaskData and 
                --    iReadModifyWriteWriteData.
                -- 2: Read data (size of a bus width) from memory via the internal bus.
                -- 3: Write the Read data to readBuffer of RMAPDecodeer a byte at a time.
                -- 4: Generate the data to write to memory from Readdata,WriteData and MaskData.
                -- 5: Write generated data to memory via an internal bus.
                ----------------------------------------------------------------------                
                case dmaState is
                    when dmaStateIdle =>
                        iBusMasterDataOut <= (others => '0');

                        if (busAccessStart = '1') then
                            
                            iReadModifyWriteAddressOut <= rmapAddress;

                            if (gBusWidth = 8) then  --08bit BusWidth
                                iBusMasterAddressOutBuffer (31 downto 0) <= rmapAddress (31 downto 0);
                                iBusMasterAddressOut (31 downto 0)       <= rmapAddress (31 downto 0);
                            end if;

                            if (gBusWidth = 16) then  --16bit BusWidth
                                iBusMasterAddressOutBuffer (31 downto 0) <= rmapAddress (31 downto 1) & '0';
                                iBusMasterAddressOut (31 downto 0)       <= rmapAddress (31 downto 1) & '0';
                            end if;

                            if (gBusWidth = 32) then  --32bit BusWidth
                                iBusMasterAddressOutBuffer (31 downto 0) <= rmapAddress (31 downto 2) & "00";
                                iBusMasterAddressOut (31 downto 0)       <= rmapAddress (31 downto 2) & "00";
                            end if;

                            ----------------------------------------------------------------------
                            -- Start Write Command operation.
                            -- Reads the data to write to memory from writeBuffer of RMAPDecodeer.
                            -- Move to the state which corresponding to the write Address.
                            ----------------------------------------------------------------------
                            if (rmapCommand (3) = '1') then
                                -- Write Command
                                iRMAPDataLength           <= rmapDataLength;
                                iDMAWriteBufferReadEnable <= '1';
                                iBusMasterByteSelectOut   <= "0000";

                                if (gBusWidth = 8) then  --08bit BusWidth
                                    dmaState <= dmaStateWrite0;
                                end if;

                                if (gBusWidth = 16) then  --16bit BusWidth
                                    if (rmapAddress (0) = '0') then
                                        dmaState <= dmaStateWrite0;
                                    else
                                        dmaState <= dmaStateWrite1;
                                    end if;
                                end if;

                                if (gBusWidth = 32) then  --32bit BusWidth
                                    if (rmapAddress (1 downto 0) = "00") then
                                        dmaState <= dmaStateWrite0;
                                        
                                    elsif (rmapAddress (1 downto 0) = "01") then
                                        dmaState <= dmaStateWrite1;
                                        
                                    elsif (rmapAddress (1 downto 0) = "10") then
                                        dmaState <= dmaStateWrite2;
                                        
                                    elsif (rmapAddress (1 downto 0) = "11") then
                                        dmaState <= dmaStateWrite3;
                                        
                                    end if;
                                end if;

                            ----------------------------------------------------------------------
                            -- Start Read command operation
                            -- To start accessing to the memory via the internal bus.
                            -- The state move to dmaStateRead0.
                            ----------------------------------------------------------------------
                            elsif (rmapCommand (2) = '0') then
                                -- Read Command
                                iRMAPDataLength                                    <= rmapDataLength;
                                iBusMasterCycleOut                                 <= '1';
                                iBusMasterStrobeOut                                <= '1';
                                iBusMasterReadEnableOut                            <= '1';
                                iBusMasterByteSelectOut((gBusWidth/8) -1 downto 0) <= (others => '1');

                                if (gBusWidth = 8) then  --08bit BusWidth
                                    iBusReadByteCount             <= 1;
                                    iDMAReadBufferWriteByteSelect <= "0001";
                                end if;

                                if (gBusWidth = 16) then  --16bit BusWidth
                                    if (rmapAddress (0) = '0') then
                                        iBusReadByteCount             <= 2;
                                        iDMAReadBufferWriteByteSelect <= "0010";
                                    end if;
                                    if (rmapAddress (0) = '1') then
                                        iBusReadByteCount             <= 1;
                                        iDMAReadBufferWriteByteSelect <= "0001";
                                    end if;
                                end if;

                                if (gBusWidth = 32) then  --32bit BusWidth
                                    if (rmapAddress (1 downto 0) = "00") then
                                        iBusReadByteCount             <= 4;
                                        iDMAReadBufferWriteByteSelect <= "1000";
                                    end if;
                                    if (rmapAddress (1 downto 0) = "01") then
                                        iBusReadByteCount             <= 3;
                                        iDMAReadBufferWriteByteSelect <= "0100";
                                    end if;
                                    if (rmapAddress (1 downto 0) = "10") then
                                        iBusReadByteCount             <= 2;
                                        iDMAReadBufferWriteByteSelect <= "0010";
                                    end if;
                                    if (rmapAddress (1 downto 0) = "11") then
                                        iBusReadByteCount             <= 1;
                                        iDMAReadBufferWriteByteSelect <= "0001";
                                    end if;
                                end if;

                                dmaState <= dmaStateRead0;

                            ----------------------------------------------------------------------
                            -- Start ReadModifyWrite Command operation.
                            -- Start to read the Write and Mask data from writeBuffer of RMAPDecodeer.
                            -- Move to dmaStateReadModifyWriteControl0 state.
                            ----------------------------------------------------------------------
                            else
                                -- read/modify/write
                                iRMAPDataLength             <= "000000000000000000000" & rmapDataLength (3 downto 1);
                                iReadModifyWriteBufferCount <= rmapDataLength (3 downto 0) - 1;
                                iReadModifyWriteByteCount   <= 1;
                                iDMAWriteBufferReadEnable   <= '1';
                                iBusMasterByteSelectOut     <= "0000";
                                dmaState                    <= dmaStateReadModifyWriteControl0;
                                --
                                iDataLength                 <= CONV_INTEGER(rmapDataLength (3 downto 1));
                            end if;
                        end if;
                        iReadModifyWriteCommand       <= '0';
                        iBusAccessEnd                 <= '0';
                        iBusMasterAcknowledgeReceived <= '0';

                    ----------------------------------------------------------------------
                    -- The wait time to read a first data byte to store the internal 
                    -- bus (iBusMasterDataOut) from writeBuffer.
                    -- If the remaining number of iRMAPDataLength is 0, or a byte of the internal 
                    -- bus width, moves to dmaStateWrite4 state.
                    -- If the remaining number of iRMAPDataLength is not 0, moves to dmaStateWrite1
                    -- state.
                    ----------------------------------------------------------------------
                    when dmaStateWrite0 =>
                        iBusMasterAcknowledgeReceived <= '0';

                        if (iRMAPDataLength > X"000000") then
                            if (dmaWriteBufferEmpty = '0') then
                                iRMAPDataLength                            <= iRMAPDataLength - 1;
                                iBusMasterByteSelectOut ((gBusWidth/8) -1) <= '1';

                                if (gBusWidth = 8) then  --08bit BusWidth
                                    iDMAWriteBufferReadEnable <= '0';
                                    dmaState                  <= dmaStateWrite4;
                                else
                                    dmaState <= dmaStateWrite1;
                                end if;
                            end if;
                        else
                            iDMAWriteBufferReadEnable <= '0';
                            dmaState                  <= dmaStateWrite4;
                        end if;

                    ----------------------------------------------------------------------
                    -- Store the a first byte data which read from writeBuffer to the internal bus (-- iBusMasterDataOut).
                    -- Read second byte to store to internal bus (iBusMasterDataOut) from 
                    -- writeBuffer.
                    --
                    -- If the remaining number of iRMAPDataLength is 0, or 2 byte of the internal 
                    -- bus width, then moves to dmaStateWrite4 state.
                    -- If the remaining number of iRMAPDataLength is not 0, moves to dmaStateWrite1
                    -- state.
                    ----------------------------------------------------------------------
                    when dmaStateWrite1 =>
                        iBusMasterDataOut ((gBusWidth)-1 downto (gBusWidth)-8) <= dmaWriteBufferReadData;

                        if (iRMAPDataLength > x"000000") then
                            if (dmaWriteBufferEmpty = '0') then
                                iRMAPDataLength <= iRMAPDataLength - 1;

                                if (gBusWidth = 16) then  --16bit BusWidth
                                    iBusMasterByteSelectOut (0) <= '1';
                                    iDMAWriteBufferReadEnable   <= '0';
                                    dmaState                    <= dmaStateWrite4;
                                    
                                elsif (gBusWidth = 32) then  --32bit BusWidth
                                    iBusMasterByteSelectOut (2) <= '1';
                                    dmaState                    <= dmaStateWrite2;
                                end if;
                            end if;
                        else
                            iDMAWriteBufferReadEnable <= '0';
                            dmaState                  <= dmaStateWrite4;
                        end if;

                    ----------------------------------------------------------------------
                    -- Store the 2nd byte data which read from writeBuffer via the internal bus.
                    -- Time to read the 3rd byte data Buffer to store internal bus (iBusMasterDataOut) from write.
                    -- If the remaining number of iRMAPDataLength is 0, moves to dmaStateWrite4 state.
                    -- If the remaining number of iRMAPDataLength is not 0, moves to dmaStateWrite3 state.
                    ----------------------------------------------------------------------    
                    when dmaStateWrite2 =>
                        iBusMasterDataOut (23 downto 16) <= dmaWriteBufferReadData;

                        if (iRMAPDataLength > x"000000") then
                            if (dmaWriteBufferEmpty = '0') then
                                iRMAPDataLength             <= iRMAPDataLength - 1;
                                iBusMasterByteSelectOut (1) <= '1';
                                dmaState                    <= dmaStateWrite3;
                            end if;
                        else
                            iDMAWriteBufferReadEnable <= '0';
                            dmaState                  <= dmaStateWrite4;
                        end if;

                    ----------------------------------------------------------------------
                    -- Store the 3rd byte data which read from writeBuffer via the internal bus.
                    -- Time of reading the 4th byte data to store internal bus (iBusMasterDataOut)
                    -- from writeBuffer.
                    -- Moves to dmaStateWrite4 state.
                    ---------------------------------------------------------------------- 
                    when dmaStateWrite3 =>
                        iBusMasterDataOut (15 downto 8) <= dmaWriteBufferReadData;

                        if (iRMAPDataLength > x"000000") then
                            if (dmaWriteBufferEmpty = '0') then
                                iRMAPDataLength             <= iRMAPDataLength - 1;
                                iBusMasterByteSelectOut (0) <= '1';
                                iDMAWriteBufferReadEnable   <= '0';
                                dmaState                    <= dmaStateWrite4;
                            end if;
                        else
                            iDMAWriteBufferReadEnable <= '0';
                            dmaState                  <= dmaStateWrite4;
                        end if;

                    ----------------------------------------------------------------------
                    -- store the 1st, 2nd or 4th byte data which read from writeBuffer to the internal bus.
                    -- Start writing to memory via the internal bus.
                    -- Move to dmaStateWrite5 state.
                    ---------------------------------------------------------------------- 
                    when dmaStateWrite4 =>

                        iBusMasterDataOut (7 downto 0) <= dmaWriteBufferReadData;
                        iBusMasterAddressOut           <= iBusMasterAddressOutBuffer;

                        if (rmapCommand(0) = '1') then
                            
                            if (gBusWidth = 8) then
                                iBusMasterAddressOutBuffer (31 downto 0) <= iBusMasterAddressOutBuffer (31 downto 0) + 1;
                            end if;

                            if (gBusWidth = 16) then
                                iBusMasterAddressOutBuffer (31 downto 1) <= iBusMasterAddressOutBuffer (31 downto 1) + 1;
                            end if;

                            if (gBusWidth = 32) then
                                iBusMasterAddressOutBuffer (31 downto 2) <= iBusMasterAddressOutBuffer (31 downto 2) + 1;
                            end if;

                        end if;
                        iBusMasterCycleOut       <= '1';
                        iBusMasterStrobeOut      <= '1';
                        iBusMasterWriteEnableOut <= '1';
                        dmaState                 <= dmaStateWrite5;

                    ----------------------------------------------------------------------
                    -- One write cycle will finish, when received ACK from the internal bus.
                    -- If the remaining number of iRMAPDataLength is not 0, moves to dmaStateWrite0
                    -- state and then repeat the write cycle again.
                    -- If the remaining number of iRMAPDataLength is 0, moves to dmaStateIdle 
                    -- state, and complite DMA operation.
                    ---------------------------------------------------------------------- 
                    when dmaStateWrite5 =>
                        
                        if (busMasterAcknowledgeIn = '1' or iBusMasterAcknowledgeReceived = '1') then
                            iBusMasterAcknowledgeReceived <= '1';
                            iBusMasterCycleOut            <= '0';
                            iBusMasterStrobeOut           <= '0';
                            iBusMasterWriteEnableOut      <= '0';
                            iBusMasterByteSelectOut       <= (others => '0');

                            if (iRMAPDataLength = x"000000") then
                                iBusAccessEnd <= '1';
                                dmaState      <= dmaStateIdle;
                            else
                                if (dmaWriteBufferEmpty = '0') then
                                    iDMAWriteBufferReadEnable     <= '1';
                                    iBusMasterAcknowledgeReceived <= '0';
                                    dmaState                      <= dmaStateWrite0;
                                end if;
                                
                            end if;
                        end if;

                    ----------------------------------------------------------------------
                    -- If received ACK from the internal bus data, store the data from the internal bus to iBusMasterDataIn. 
                    --                            (Store maximum 4 bytes Simultaneously)
                    -- Outputs a iDMAReadBufferWriteEnable and Move to dmaStateRead2 state to write
                    -- the data from the internal bus to readBuffer if there are space in readBuffer.
                    -- If readBuffer is FULL, move to dmaStateRead1 state.
                    ---------------------------------------------------------------------- 
                    when dmaStateRead0 =>
                        if (busMasterAcknowledgeIn = '1') then
                            iBusMasterCycleOut      <= '0';
                            iBusMasterStrobeOut     <= '0';
                            iBusMasterReadEnableOut <= '0';

                            if (rmapCommand(0) = '1') then
                                iBusMasterAddressOutBuffer (31 downto 0) <= iBusMasterAddressOutBuffer (31 downto 0) + (gBusWidth/8);
                            end if;

                            iBusMasterDataIn <= busMasterDataIn;

                            if (dmaReadBufferWriteReady = '1') then
                                iBusReadByteCount         <= iBusReadByteCount - 1;
                                iDMAReadBufferWriteEnable <= '1';
                                iRMAPDataLength           <= iRMAPDataLength - 1;
                                dmaState                  <= dmaStateRead2;
                            else
                                dmaState <= dmaStateRead1;
                            end if;
                        end if;

                    ----------------------------------------------------------------------
                    -- Outputs a iDMAReadBufferWriteEnable and Move to dmaStateRead2 state to write
                    -- the data from the internal bus to readBuffer if there are space in readBuffer.
                    ---------------------------------------------------------------------- 
                    when dmaStateRead1 =>
                        if (dmaReadBufferWriteReady = '1') then
                            iBusReadByteCount         <= iBusReadByteCount - 1;
                            iDMAReadBufferWriteEnable <= '1';
                            iRMAPDataLength           <= iRMAPDataLength - 1;
                            dmaState                  <= dmaStateRead2;
                        end if;

                    ----------------------------------------------------------------------
                    -- Time to write data to readBuffer.
                    -- Move to dmaStateRead3 state.
                    ---------------------------------------------------------------------- 
                    when dmaStateRead2 =>
                        iDMAReadBufferWriteEnable <= '0';
                        dmaState                  <= dmaStateRead3;

                    ----------------------------------------------------------------------
                    -- Repeat the dmaStateRead2 to dmaStateRead3 state until cpmplete 
                    -- (iBusReadByteCount = 0)to write all data(iBusMasterDataIn) which 
                    -- read at dmaStateRead0 to readBuffer.
                    -- Move to dmaStateRead4 state after complete to write the data.
                    -- Move to dmaStateIdle state if the remaining number of iRMAPDataLength is 0,
                    -- and cpmplete DMA operation.
                    ---------------------------------------------------------------------- 
                    when dmaStateRead3 =>
                        if (iRMAPDataLength = x"000000") then
                            
                            if (iReadModifyWriteCommand = '1') then
                                iReadModifyWriteCommand <= '0';
                                dmaState                <= dmaStateReadModifyWriteWrite0;
                            else
                                iBusAccessEnd <= '1';
                                dmaState      <= dmaStateIdle;
                            end if;
                            
                        else
                            if (iBusReadByteCount = 0) then
                                dmaState <= dmaStateRead4;
                                
                            elsif (dmaReadBufferWriteReady = '1') then
                                iBusReadByteCount             <= iBusReadByteCount - 1;
                                iDMAReadBufferWriteByteSelect <= iDMAReadBufferWriteByteSelect(0) & iDMAReadBufferWriteByteSelect(3 downto 1);
                                iDMAReadBufferWriteEnable     <= '1';

                                iRMAPDataLength <= iRMAPDataLength - 1;

                                dmaState <= dmaStateRead2;
                                
                            else
                                iDMAReadBufferWriteEnable <= '0';
                            end if;

                        end if;

                    ----------------------------------------------------------------------
                    -- Start Next ReadCycle
                    -- Start to read the data via the internal bus from memory.
                    -- Move to dmaStateRead0 state.
                    ---------------------------------------------------------------------- 
                    when dmaStateRead4 =>
                        iBusMasterCycleOut      <= '1';
                        iBusMasterStrobeOut     <= '1';
                        iBusMasterReadEnableOut <= '1';
                        iBusMasterAddressOut    <= iBusMasterAddressOutBuffer;

                        if (gBusWidth = 8) then
                            iBusReadByteCount             <= 1;
                            iDMAReadBufferWriteByteSelect <= "0001";
                        end if;
                        if (gBusWidth = 16) then
                            iBusReadByteCount             <= 2;
                            iDMAReadBufferWriteByteSelect <= "0010";
                        end if;
                        if (gBusWidth = 32) then
                            iBusReadByteCount             <= 4;
                            iDMAReadBufferWriteByteSelect <= "1000";
                        end if;

                        dmaState <= dmaStateRead0;


                    ---------------------------------------------------------------------- 
                    -- Time to read written data(iReadModifyWriteWriteData) from writeBuffer.
                    ----------------------------------------------------------------------                     
                    when dmaStateReadModifyWriteControl0 =>
                        dmaState                      <= dmaStateReadModifyWriteControl1;
                        iReadModifyWriteBufferCount   <= iReadModifyWriteBufferCount - 1;
                        iReadModifyWriteReadDataCount <= 0;

                    ---------------------------------------------------------------------- 
                    -- Store the WriteData which read from writeBuffer to iReadModifyWriteWriteData sequentially.
                    ----------------------------------------------------------------------                             
                    when dmaStateReadModifyWriteControl1 =>
                        iReadModifyWriteByteCount <= iReadModifyWriteByteCount + 1;
                        if (iReadModifyWriteByteCount = iRMAPDataLength) then
                            iReadModifyWriteByteCount <= 1;
                            dmaState                  <= dmaStateReadModifyWriteControl2;
                        end if;

                        iReadModifyWriteWriteData(32-(iReadModifyWriteByteCount*8)+7 downto 32-(iReadModifyWriteByteCount*8)) <= dmaWriteBufferReadData;

                        if (iReadModifyWriteBufferCount = "0000") then
                            iDMAWriteBufferReadEnable <= '0';
                        else
                            iReadModifyWriteBufferCount <= iReadModifyWriteBufferCount - 1;
                        end if;

                    ---------------------------------------------------------------------- 
                    -- Store the MaskData which read from writeBuffer to iReadModifyWriteMaskData sequentially.
                    -- Move to dmaStateRead0 state, after read WriteData and MaskData.
                    ----------------------------------------------------------------------   
                    when dmaStateReadModifyWriteControl2 =>
                        iReadModifyWriteByteCount <= iReadModifyWriteByteCount + 1;

                        if (iReadModifyWriteBufferCount = "0000") then
                            iDMAWriteBufferReadEnable <= '0';
                        else
                            iReadModifyWriteBufferCount <= iReadModifyWriteBufferCount - 1;
                        end if;

                        if (iReadModifyWriteByteCount = iRMAPDataLength) then
                            iReadModifyWriteByteCount                         <= 1;
                            -- bus read start
                            iBusMasterCycleOut                                <= '1';
                            iBusMasterStrobeOut                               <= '1';
                            iBusMasterReadEnableOut                           <= '1';
                            iBusMasterByteSelectOut((gBusWidth/8)-1 downto 0) <= (others => '1');

                            if (gBusWidth = 8) then
                                iBusReadByteCount             <= 1;
                                iDMAReadBufferWriteByteSelect <= "0001";
                            end if;

                            if (gBusWidth = 16) then
                                if (rmapAddress (0) = '0') then
                                    iBusReadByteCount             <= 2;
                                    iDMAReadBufferWriteByteSelect <= "0010";
                                end if;
                                if (rmapAddress (0) = '1') then
                                    iBusReadByteCount             <= 1;
                                    iDMAReadBufferWriteByteSelect <= "0001";
                                end if;
                            end if;

                            if (gBusWidth = 32) then
                                if (rmapAddress (1 downto 0) = "00") then
                                    iBusReadByteCount             <= 4;
                                    iDMAReadBufferWriteByteSelect <= "1000";
                                end if;
                                if (rmapAddress (1 downto 0) = "01") then
                                    iBusReadByteCount             <= 3;
                                    iDMAReadBufferWriteByteSelect <= "0100";
                                end if;
                                if (rmapAddress (1 downto 0) = "10") then
                                    iBusReadByteCount             <= 2;
                                    iDMAReadBufferWriteByteSelect <= "0010";
                                end if;
                                if (rmapAddress (1 downto 0) = "11") then
                                    iBusReadByteCount             <= 1;
                                    iDMAReadBufferWriteByteSelect <= "0001";
                                end if;
                            end if;

                            iReadModifyWriteCommand    <= '1';
                            iReadModifyWriteDataLength <= iRMAPDataLength (2 downto 0);  -- save
                            dmaState                   <= dmaStateRead0;
                        end if;
                        iReadModifyWriteMaskData(32-(iReadModifyWriteByteCount*8)+7 downto 32-(iReadModifyWriteByteCount*8)) <= dmaWriteBufferReadData;

                    ---------------------------------------------------------------------- 
                    -- Generate the data to write to memory from ReadData, WriteData and MaskData.
                    ---------------------------------------------------------------------- 
                    when dmaStateReadModifyWriteWrite0 =>
                        iReadModifyWriteByteCount <= 0;
                        iBusMasterByteSelectOut   <= (others => '0');

                        iReadModifyWriteWriteData <= (iReadModifyWriteWriteData and iReadModifyWriteMaskData) or
                                                     (iReadModifyWriteReadData and not iReadModifyWriteMaskData);

                        if (gBusWidth = 8) then
                            iBusMasterAddressOut <= iReadModifyWriteAddressOut;
                        end if;

                        if (gBusWidth = 16) then
                            iBusMasterAddressOut <= iReadModifyWriteAddressOut(31 downto 1) & '0';
                        end if;

                        if (gBusWidth = 32) then
                            iBusMasterAddressOut <= iReadModifyWriteAddressOut(31 downto 2) & "00";
                        end if;

                        dmaState <= dmaStateReadModifyWriteWrite1;

                    ---------------------------------------------------------------------- 
                    -- Prepare the data (ReadModifyWriteDataOut) suit with bus width and address 
                    -- to write to memory from iReadModifyWriteWriteData.
                    ---------------------------------------------------------------------- 
                    when dmaStateReadModifyWriteWrite1 =>

                        if (gBusWidth = 8) then  --08bit BusWidth
                            iReadModifyWriteByteCount             <= iReadModifyWriteByteCount + 1;
                            iBusMasterByteSelectOut(0)            <= '1';
                            iReadModifyWriteDataOut (31 downto 8) <= (others => '0');
                            iReadModifyWriteDataOut (7 downto 0)  <=
                                iReadModifyWriteWriteData(31-(iReadModifyWriteByteCount*8) downto 24-(iReadModifyWriteByteCount*8));
                            
                            dmaState <= dmaStateReadModifyWriteWrite2;
                        end if;

                        if (gBusWidth = 16) then  --16bit BusWidth
                            
                            if (iReadModifyWriteAddressOut(0) = '0') then
                                
                                if (iReadModifyWriteDataLength(0) = '1') then  --1Byte : Read_Modify_Write
                                    iReadModifyWriteDataOut (15 downto 0) <= iReadModifyWriteWriteData(31 downto 24) & x"00";

                                    iReadModifyWriteByteCount  <= iReadModifyWriteByteCount + 1;
                                    iBusMasterByteSelectOut(1) <= '1';
                                end if;

                                if (iReadModifyWriteDataLength(0) = '0') then  --2Byte or 4Byte : Read_Modify_Write
                                    iReadModifyWriteDataOut (15 downto 0) <=
                                        iReadModifyWriteWriteData(31-(iReadModifyWriteByteCount*8) downto 16-(iReadModifyWriteByteCount*8));
                                    
                                    iReadModifyWriteByteCount           <= iReadModifyWriteByteCount + 2;
                                    iBusMasterByteSelectOut(1 downto 0) <= "11";
                                end if;
                                
                            else        --1Byte : Read_Modify_Write
                                iReadModifyWriteDataOut (15 downto 0) <= x"00" & iReadModifyWriteWriteData(31 downto 24);

                                iReadModifyWriteByteCount  <= iReadModifyWriteByteCount + 1;
                                iBusMasterByteSelectOut(0) <= '1';
                            end if;

                            iReadModifyWriteDataOut (31 downto 16) <= x"0000";
                            dmaState                               <= dmaStateReadModifyWriteWrite2;
                            
                        end if;

                        if (gBusWidth = 32) then  --32bit BusWidth
                            case iReadModifyWriteAddressOut(1 downto 0) is
                                when "00" =>
                                    iReadModifyWriteDataOut (31 downto 0) <= iReadModifyWriteWriteData (31 downto 0);

                                    if (iReadModifyWriteDataLength = "001") then
                                        iReadModifyWriteByteCount  <= iReadModifyWriteByteCount + 1;
                                        iBusMasterByteSelectOut(3) <= '1';
                                    end if;

                                    if (iReadModifyWriteDataLength = "010") then
                                        iReadModifyWriteByteCount           <= iReadModifyWriteByteCount + 2;
                                        iBusMasterByteSelectOut(3 downto 2) <= "11";
                                    end if;

                                    if (iReadModifyWriteDataLength = "100") then
                                        iReadModifyWriteByteCount <= iReadModifyWriteByteCount + 4;
                                        iBusMasterByteSelectOut   <= "1111";
                                    end if;
                                    
                                when "01" =>
                                    iReadModifyWriteDataOut (31 downto 0) <= x"00" & iReadModifyWriteWriteData (31 downto 24) & x"0000";

                                    if (iReadModifyWriteDataLength = "001") then
                                        iReadModifyWriteByteCount  <= iReadModifyWriteByteCount + 1;
                                        iBusMasterByteSelectOut(2) <= '1';
                                    end if;
                                    
                                when "10" =>
                                    iReadModifyWriteDataOut (31 downto 0) <= x"0000" & iReadModifyWriteWriteData (31 downto 16);

                                    if (iReadModifyWriteDataLength = "001") then
                                        iReadModifyWriteByteCount  <= iReadModifyWriteByteCount + 1;
                                        iBusMasterByteSelectOut(1) <= '1';
                                    end if;

                                    if (iReadModifyWriteDataLength = "010") then
                                        iReadModifyWriteByteCount           <= iReadModifyWriteByteCount + 2;
                                        iBusMasterByteSelectOut(1 downto 0) <= "11";
                                    end if;
                                    
                                when "11" =>
                                    iReadModifyWriteDataOut (31 downto 0) <= x"000000" & iReadModifyWriteWriteData (31 downto 24);

                                    if (iReadModifyWriteDataLength = "001") then
                                        iReadModifyWriteByteCount  <= iReadModifyWriteByteCount + 1;
                                        iBusMasterByteSelectOut(0) <= '1';
                                    end if;
                                    
                                when others => null;
                            end case;

                            dmaState <= dmaStateReadModifyWriteWrite2;
                        end if;

                    ---------------------------------------------------------------------- 
                    -- Start to write iReadModifyWriteDataOut to memory.
                    ---------------------------------------------------------------------- 
                    when dmaStateReadModifyWriteWrite2 =>
                        iBusMasterDataOut        <= iReadModifyWriteDataOut;
                        iBusMasterCycleOut       <= '1';
                        iBusMasterStrobeOut      <= '1';
                        iBusMasterWriteEnableOut <= '1';

                        dmaState <= dmaStateReadModifyWriteWrite3;

                    ---------------------------------------------------------------------- 
                    -- Complete ReadModifyWrite when received ACK.
                    ---------------------------------------------------------------------- 
                    when dmaStateReadModifyWriteWrite3 =>
                        if (busMasterAcknowledgeIn = '1') then
                            
                            iBusMasterByteSelectOut  <= (others => '0');
                            iBusMasterCycleOut       <= '0';
                            iBusMasterStrobeOut      <= '0';
                            iBusMasterWriteEnableOut <= '0';

                            if (gBusWidth = 8) then
                                iBusMasterAddressOut <= iBusMasterAddressOut + 1;
                            end if;

                            if (gBusWidth = 16) then
                                iBusMasterAddressOut <= iBusMasterAddressOut + 2;
                            end if;

                            if (iReadModifyWriteByteCount < iDataLength) then
                                dmaState <= dmaStateReadModifyWriteWrite1;
                            else
                                iBusAccessEnd <= '1';
                                dmaState      <= dmaStateIdle;
                            end if;

                        end if;
                        
                    when others => null;
                                   
                end case;
            end if;

            if (iReadModifyWriteCommand = '1' and iDMAReadBufferWriteEnable = '1') then
                iReadModifyWriteReadDataCount                                                                              <= iReadModifyWriteReadDataCount + 1;
                iReadModifyWriteReadData(31 - 8*iReadModifyWriteReadDataCount downto 24 - 8*iReadModifyWriteReadDataCount) <= idmaReadBufferWriteData;
            end if;
------------------------------------------
        end if;
        
    end process;

    busMasterCycleOut              <= iBusMasterCycleOut;
    busMasterStrobeOut             <= iBusMasterStrobeOut;
    busMasterAddressOut            <= iBusMasterAddressOut;
    busMasterByteEnableOut         <= iBusMasterByteSelectOut;
    busMasterDataOut(31 downto 24) <= iBusMasterDataOut(31 downto 24) when iBusMasterByteSelectOut(3) = '1' else (others => '0');
    busMasterDataOut(23 downto 16) <= iBusMasterDataOut(23 downto 16) when iBusMasterByteSelectOut(2) = '1' else (others => '0');
    busMasterDataOut(15 downto 8)  <= iBusMasterDataOut(15 downto 8)  when iBusMasterByteSelectOut(1) = '1' else (others => '0');
    busMasterDataOut(7 downto 0)   <= iBusMasterDataOut(7 downto 0)   when iBusMasterByteSelectOut(0) = '1' else (others => '0');
    busMasterWriteEnableOut        <= iBusMasterWriteEnableOut;
    busMasterReadEnableOut         <= iBusMasterReadEnableOut;

end behavioral;

