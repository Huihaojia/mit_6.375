import FIFO :: *; 
import FixedPoint :: *;
import Vector :: *;
import Multiplier::*;

import AudioProcessorTypes :: *;
import FilterCoefficients :: *;

// The FIR Filter Module Definition 

module mkFIRFilter(AudioProcessor);
    FIFO#(Sample) infifo <- mkFIFO();
    FIFO#(Sample) outfifo <- mkFIFO();

    Vector#(8, Reg#(Sample)) r <- replicateM(mkReg(0));
    Vector#(9, Multiplier) multiplier <- replicateM(mkMultiplier());

    //所有子模块rdy满足才会fire
    
    rule process0;
        Sample sample = infifo.first();
        infifo.deq();

        r[0] <= sample;
        for (Integer i = 1; i < 8; i = i + 1) begin
            r[i] <= r [i-1];
        end

        multiplier[0].putOperands(c[0], sample);
        for (Integer i = 1; i < 9; i = i + 1) begin
            if (multiplier.fifo.notFull)
                multiplier[i].putOperands(c[i], r[i-1]);
            if (multiplier.fifo.notEmpty)
                let x <- multiplier[i].getResult();
            accumulate = accumulate + x;
        end
        // fxptGetInt: Extract the integer part of fixed point number
    endrule

    // rule process1;
    //     FixedPoint#(16, 16) accumulate = 0;
    //     for (Integer i = 0; i < 9; i = i + 1) begin
    //         let x <- multiplier[i].getResult();
    //         accumulate = accumulate + x;
    //     end
    //     outfifo.enq(fxptGetInt(accumulate));
    // endrule

    // rule process1;
    //     multiplier[1].putOperands(c[1], r[0]);
    //     let x <- multiplier[1].getResult();
    //     accumulate = accumulate + x;
    // endrule

    // rule process2;
    //     multiplier[2].putOperands(c[2], r[1]);
    //     let x <- multiplier[2].getResult();
    //     accumulate = accumulate + x;
    // endrule

    // rule process3;
    //     multiplier[3].putOperands(c[3], r[2]);
    //     let x <- multiplier[3].getResult();
    //     accumulate = accumulate + x;
    // endrule

    // rule process4;
    //     multiplier[4].putOperands(c[4], r[3]);
    //     let x <- multiplier[4].getResult();
    //     accumulate = accumulate + x;
    // endrule

    // rule process5;
    //     multiplier[5].putOperands(c[5], r[4]);
    //     let x <- multiplier[5].getResult();
    //     accumulate = accumulate + x;
    // endrule

    // rule process6;
    //     multiplier[6].putOperands(c[6], r[5]);
    //     let x <- multiplier[6].getResult();
    //     accumulate = accumulate + x;
    // endrule

    // rule process7;
    //     multiplier[7].putOperands(c[7], r[6]);
    //     let x <- multiplier[7].getResult();
    //     accumulate = accumulate + x;
    // endrule

    // rule process8;
    //     multiplier[8].putOperands(c[8], r[7]);
    //     let x <- multiplier[8].getResult();
    //     accumulate = accumulate + x;
    //     outfifo.enq(fxptGetInt(accumulate));
    // endrule

    method Action putSampleInput(Sample in);
        infifo.enq(in);
    endmethod

    method ActionValue#(Sample) getSampleOutput();
        outfifo.deq();
        return outfifo.first();
    endmethod
endmodule


module mkFIRFilter4_2(AudioProcessor);
    FIFO#(Sample) infifo <- mkFIFO();
    FIFO#(Sample) outfifo <- mkFIFO();

    Vector#(8, Reg#(Sample)) r <- replicateM(mkReg(0));
    Vector#(9, Multiplier) multiplier <- replicateM(mkMultiplier());

    rule process (True);
        Sample sample = infifo.first();
        infifo.deq();

        r[0] <= sample;
        for (Integer i = 1; i < 8; i = i + 1) begin
            r[i] <= r [i-1];
        end

        FixedPoint#(16, 16) accumulate = c[0] * fromInt(sample);
        for (Integer i = 1; i < 9; i = i + 1) begin
            accumulate = accumulate + c[i] * fromInt(r[i-1]);
        end
        // fxptGetInt: Extract the integer part of fixed point number
        outfifo.enq(fxptGetInt(accumulate));
    endrule

    method Action putSampleInput(Sample in);
        infifo.enq(in);
    endmethod

    method ActionValue#(Sample) getSampleOutput();
        outfifo.deq();
        return outfifo.first();
    endmethod
endmodule