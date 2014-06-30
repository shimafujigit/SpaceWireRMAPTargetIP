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



entity RMAPTargetIPBufferFIFO8x2kAltera is
    port (
        clock       : in  std_logic;
        DataIn      : in  std_logic_vector (7 downto 0);
        readEnable  : in  std_logic;
        reset       : in  std_logic;
        writeEnable : in  std_logic;
        dataCount   : out std_logic_vector (10 downto 0);
        dataOut     : out std_logic_vector (7 downto 0);
        empty       : out std_logic;
        full        : out std_logic
        );
end RMAPTargetIPBufferFIFO8x2kAltera;

architecture Behavioral of RMAPTargetIPBufferFIFO8x2kAltera is

----------------------------------------------------------------------
-- Altera IP Wrapper File.
----------------------------------------------------------------------

    component FIFO8x2kAltera is
        port (
            clk        : in  std_logic;
            din        : in  std_logic_vector (7 downto 0);
            rd_en      : in  std_logic;
            rst        : in  std_logic;
            wr_en      : in  std_logic;
            data_count : out std_logic_vector (10 downto 0);
            dout       : out std_logic_vector (7 downto 0);
            empty      : out std_logic;
            full       : out std_logic
            );
    end component;

begin

    bufferFIFO : FIFO8x2kAltera port map (
        clk        => clock,
        rst        => reset,
        wr_en      => writeEnable,
        din        => DataIn,
        rd_en      => readEnable,
        dout       => dataOut,
        full       => full,
        empty      => empty,
        data_count => dataCount
        );



end Behavioral;

