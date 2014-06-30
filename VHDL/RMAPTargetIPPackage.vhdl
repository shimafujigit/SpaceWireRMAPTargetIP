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

package RMAPTargetIPPackage is

-- ============================================================================
--  Declare constants
-- ============================================================================
    constant cBusWidth        : integer range 8 to 32 := 8;  -- 8 = 8bit, 16 = 16bit, 32 = 32bit,
    constant cRMAPCRCRevision : std_logic             := '1';  --(0:Rev.e, 1:Rev.f)
    constant cUseDevice       : integer range 0 to 1  := 1;  -- 0 = Altera,  1 = Xilinx

-- ============================================================================
--  Declare State Mchine
-- ============================================================================
    type commandStateMachine is (
        commandStateIdle,
        commandStateTargetLogicalAddress,
        commandStateProtocolID,
        commandStateInstruction,
        commandStateKey,
        commandStateReplyAddress,
        commandStateInitiatorLogicalAddress,
        commandStateTransactionID,
        commandStateExtendedAddress,
        commandStateAddress,
        commandStateDataLength,
        commandStateHeaderCRC,
        commandStateData,
        commandStateDataCRC,
        commandStateDataEOP,
        commandStateWaitEOP,
        commandStateWaitBusAccessEnd
        );

    type replyStateMachine is (
        replyStateIdle,
        replyStatePathAddress,
        replyStateInitiatorLogicalAddress,
        replyStateProtocolID,
        replyStateInstruction,
        replyStateStatus,
        replyStateTargetLogicalAddress,
        replyStateTransactionID,
        replyStateReservedByte,
        replyStateDataLength,
        replyStateHeaderCRC,
        replyStateDataWait,
        replyStateData,
        replyStateDataCRC,
        replyStateEOP
        );

end RMAPTargetIPPackage;

package body RMAPTargetIPPackage is

end RMAPTargetIPPackage;
