
import ClientServer::*;
import FIFO::*;
import GetPut::*;

import FixedPoint::*;
import Vector::*;

import ComplexMP::*;
import Complex::*;
import Cordic::*;


typedef Server#(
    Vector#(nbins, ComplexMP#(isize, fsize, psize)),
    Vector#(nbins, ComplexMP#(isize, fsize, psize))
) PitchAdjust#(numeric type nbins, numeric type isize, numeric type fsize, numeric type psize);


// s - the amount each window is shifted from the previous window.
//
// factor - the amount to adjust the pitch.
//  1.0 makes no change. 2.0 goes up an octave, 0.5 goes down an octave, etc...
module mkPitchAdjust(
        Integer s,
        FixedPoint#(isize, fsize) factor,
        PitchAdjust#(nbins, isize, fsize, psize) ifc
    ) provisos (
        Add#(a__, psize, TAdd#(isize, isize)),
        Add#(psize, c__, isize));

    FIFO#(Vector#(nbins, ComplexMP#(isize, fsize, psize))) inFifo <- mkFIFO();
    FIFO#(Vector#(nbins, ComplexMP#(isize, fsize, psize))) outFifo <- mkFIFO();

    FIFO#(Vector#(nbins, FixedPoint#(isize, fsize))) magFifo <- mkFIFO();
    FIFO#(Vector#(nbins, Phase#(psize))) dphaseFifo <- mkFIFO();

    rule dphase_calculate;
        inFifo.deq();
        let inPhases = inFifo.first();
        Vector#(nbins, FixedPoint#(isize, fsize)) mags;
        Vector#(nbins, Phase#(psize)) dphases;

        mags[0] = inPhases[0].magnitude;
        dphases[0] = inPhases[0].phase;
        for (Integer i = 1; i < valueOf(nbins); i = i + 1) begin
            mags[i] = inPhases[i].magnitude;
            dphases[i] = inPhases[i].phase;
        end
        magFifo.enq(mags);
        dphaseFifo.enq(dphases);
    endrule

    rule result_index_calculate;
        let mags = magFifo.first();
        magFifo.deq();
        let dphases = dphaseFifo.first();
        dphaseFifo.deq();
        
        Vector#(nbins, ComplexMP#(isize, fsize, psize)) outCmplx;

        outCmplx = replicate(cmplxmp(0.0, 0));

        for (Integer i = 0; i < valueOf(nbins); i = i + 1) begin
            Int#(isize) i_int = fromInteger(i);
            Int#(isize) ni_int = i_int + 1;

            FixedPoint#(isize, fsize) index = fromInt(i_int);
            FixedPoint#(isize, fsize) nindex = fromInt(ni_int);
            let bin_r = fxptMult(factor, index);
            let nbin_r = fxptMult(factor, nindex);
            Int#(isize) bin = truncate(fxptGetInt(bin_r));
            Int#(isize) nbin = truncate(fxptGetInt(nbin_r));
            if(bin != nbin && bin < fromInteger(valueOf(nbins))) begin
                FixedPoint#(isize, fsize) temp = fromInt(dphases[i]);
                Phase#(psize) phaseTemp = truncate(fxptGetInt(fxptMult(temp, factor)));
                outCmplx[bin] = cmplxmp(mags[i], phaseTemp);
            end
        end

        outFifo.enq(outCmplx);
    endrule

    interface Put request;
        method Action put(Vector#(nbins, ComplexMP#(isize, fsize, psize)) in);
            inFifo.enq(in);
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(Vector#(nbins, ComplexMP#(isize, fsize, psize))) get();
            outFifo.deq();
            return outFifo.first();
        endmethod
    endinterface
endmodule

typedef Server#(
    Vector#(nbins, Complex#(FixedPoint#(isize, fsize))),
    Vector#(nbins, ComplexMP#(isize, fsize, psize))
) ToMP#(numeric type nbins, numeric type isize, numeric type fsize, numeric type psize);

module mkToMp(ToMP#(nbins, isize, fsize, psize));
    FIFO#(Vector#(nbins, Complex#(FixedPoint#(isize, fsize)))) inFifo <- mkFIFO();
    FIFO#(Vector#(nbins, ComplexMP#(isize, fsize, psize))) outFifo <- mkFIFO();

    Vector#(nbins, ToMagnitudePhase#(isize, fsize, psize)) ctps <- replicateM(mkCordicToMagnitudePhase());

    rule puts;
        inFifo.deq();
        let in = inFifo.first();
        for (Integer i = 0; i < valueOf(nbins); i = i + 1) begin
            ctps[i].request.put(in[i]);
        end
    endrule

    rule gets;
        Vector#(nbins, ComplexMP#(isize, fsize, psize)) results;
        for (Integer i = 0; i < valueOf(nbins); i = i + 1) begin
            results[i] <- ctps[i].response.get();
        end
        outFifo.enq(results);
    endrule
    
    interface Put request;
        method Action put(Vector#(nbins, Complex#(FixedPoint#(isize, fsize))) x);
            inFifo.enq(x);
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(Vector#(nbins, ComplexMP#(isize, fsize, psize))) get();
            outFifo.deq();
            return outFifo.first();
        endmethod
    endinterface
endmodule

typedef Server#(
    Vector#(nbins, ComplexMP#(isize, fsize, psize)),
    Vector#(nbins, Complex#(FixedPoint#(isize, fsize)))
) FromMP#(numeric type nbins, numeric type isize, numeric type fsize, numeric type psize);

module mkFromMp(FromMP#(nbins, isize, fsize, psize));
    FIFO#(Vector#(nbins, ComplexMP#(isize, fsize, psize))) inFifo <- mkFIFO();
    FIFO#(Vector#(nbins, Complex#(FixedPoint#(isize, fsize)))) outFifo <- mkFIFO();

    Vector#(nbins, FromMagnitudePhase#(isize, fsize, psize)) ptcs <- replicateM(mkCordicFromMagnitudePhase());

    rule puts;
        inFifo.deq();
        let in = inFifo.first();
        for (Integer i = 0; i < valueOf(nbins); i = i + 1) begin
            ptcs[i].request.put(in[i]);
        end
    endrule

    rule gets;
        Vector#(nbins, Complex#(FixedPoint#(isize, fsize))) results;
        for (Integer i = 0; i < valueOf(nbins); i = i + 1) begin
            results[i] <- ptcs[i].response.get();
        end
        outFifo.enq(results);
    endrule
    
    interface Put request;
        method Action put(Vector#(nbins, ComplexMP#(isize, fsize, psize)) x);
            inFifo.enq(x);
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(Vector#(nbins, Complex#(FixedPoint#(isize, fsize)))) get();
            outFifo.deq();
            return outFifo.first();
        endmethod
    endinterface
endmodule
