import FIFO :: *; 
import FixedPoint :: *;
import Vector :: *;
import Multiplier::*;

import AudioProcessorTypes :: *;
import FilterCoefficients :: *;

// The FIR Filter Module Definition 

// Polymorphic Filter
module mkFIRFilter(Vector#(9, FixedPoint#(16, 16)) coeffs, AudioProcessor ifc);

// module mkFIRFilter(AudioProcessor);
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
            multiplier[i].putOperands(c[i], r[i-1]);
        end
        // fxptGetInt: Extract the integer part of fixed point number
    endrule

    rule process1;
        FixedPoint#(16, 16) accumulate = 0;
        for (Integer i = 0; i < 9; i = i + 1) begin
            let x <- multiplier[i].getResult();
            accumulate = accumulate + x;
        end
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