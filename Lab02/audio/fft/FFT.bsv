
// import ClientServer::*;
// import Complex::*;
// import FIFO::*;
// import FIFOF::*;
// import Reg6375::*;
// import GetPut::*;
// import Real::*;
// import Vector::*;

// import AudioProcessorTypes::*;

// typedef Server#(
//     Vector#(FFT_POINTS, ComplexSample),
//     Vector#(FFT_POINTS, ComplexSample)
// ) FFT;

// // Get the appropriate twiddle factor for the given stage and index.
// // This computes the twiddle factor statically.
// function ComplexSample getTwiddle(Integer stage, Integer index, Integer points);
//     Integer i = ((2*index)/(2 ** (log2(points)-stage))) * (2 ** (log2(points)-stage));
//     return cmplx(fromReal(cos(fromInteger(i)*pi/fromInteger(points))),
//                  fromReal(-1*sin(fromInteger(i)*pi/fromInteger(points))));
// endfunction

// // Generate a table of all the needed twiddle factors.
// // The table can be used for looking up a twiddle factor dynamically.
// typedef Vector#(FFT_LOG_POINTS, Vector#(TDiv#(FFT_POINTS, 2), ComplexSample)) TwiddleTable;
// function TwiddleTable genTwiddles();
//     TwiddleTable twids = newVector;
//     for (Integer s = 0; s < valueof(FFT_LOG_POINTS); s = s+1) begin
//         for (Integer i = 0; i < valueof(TDiv#(FFT_POINTS, 2)); i = i+1) begin
//             twids[s][i] = getTwiddle(s, i, valueof(FFT_POINTS));
//         end
//     end
//     return twids;
// endfunction

// // Given the destination location and the number of points in the fft, return
// // the source index for the permutation.
// function Integer permute(Integer dst, Integer points);
//     Integer src = ?;
//     if (dst < points/2) begin
//         src = dst*2;
//     end else begin
//         src = (dst - points/2)*2 + 1;
//     end
//     return src;
// endfunction

// // Reorder the given vector by swapping words at positions
// // corresponding to the bit-reversal of their indices.
// // The reordering can be done either as as the
// // first or last phase of the FFT transformation.
// function Vector#(FFT_POINTS, ComplexSample) bitReverse(Vector#(FFT_POINTS,ComplexSample) inVector);
//     Vector#(FFT_POINTS, ComplexSample) outVector = newVector();
//     for(Integer i = 0; i < valueof(FFT_POINTS); i = i+1) begin   
//         Bit#(FFT_LOG_POINTS) reversal = reverseBits(fromInteger(i));
//         outVector[reversal] = inVector[i];           
//     end  
//     return outVector;
// endfunction

// // 2-way Butterfly
// function Vector#(2, ComplexSample) bfly2(Vector#(2, ComplexSample) t, ComplexSample k);
//     ComplexSample m = t[1] * k;

//     Vector#(2, ComplexSample) z = newVector();
//     z[0] = t[0] + m;
//     z[1] = t[0] - m; 

//     return z;
// endfunction

// // Perform a single stage of the FFT, consisting of butterflys and a single
// // permutation.
// // We pass the table of twiddles as an argument so we can look those up
// // dynamically if need be.
// function Vector#(FFT_POINTS, ComplexSample) stage_ft(TwiddleTable twiddles, Bit#(TLog#(FFT_LOG_POINTS)) stage, Vector#(FFT_POINTS, ComplexSample) stage_in);
//     Vector#(FFT_POINTS, ComplexSample) stage_temp = newVector();
//     for(Integer i = 0; i < (valueof(FFT_POINTS)/2); i = i+1) begin    
//         Integer idx = i * 2;
//         let twid = twiddles[stage][i];
//         let y = bfly2(takeAt(idx, stage_in), twid);

//         stage_temp[idx]   = y[0];
//         stage_temp[idx+1] = y[1];
//     end 

//     Vector#(FFT_POINTS, ComplexSample) stage_out = newVector();
//     for (Integer i = 0; i < valueof(FFT_POINTS); i = i+1) begin
//         stage_out[i] = stage_temp[permute(i, valueof(FFT_POINTS))];
//     end
//     return stage_out;
// endfunction

// module mkCombinationalFFT (FFT);

//   // Statically generate the twiddle factors table.
//   TwiddleTable twiddles = genTwiddles();

//   // Define the stage_f function which uses the generated twiddles.
//   function Vector#(FFT_POINTS, ComplexSample) stage_f(Bit#(TLog#(FFT_LOG_POINTS)) stage, Vector#(FFT_POINTS, ComplexSample) stage_in);
//       return stage_ft(twiddles, stage, stage_in);
//   endfunction

//   FIFO#(Vector#(FFT_POINTS, ComplexSample)) inputFIFO  <- mkFIFO(); 
//   FIFO#(Vector#(FFT_POINTS, ComplexSample)) outputFIFO <- mkFIFO(); 

//   // This rule performs fft using a big mass of combinational logic.
//   rule comb_fft;

//     Vector#(TAdd#(1, FFT_LOG_POINTS), Vector#(FFT_POINTS, ComplexSample)) stage_data = newVector();
//     stage_data[0] = inputFIFO.first();
//     inputFIFO.deq();

//     for(Integer stage = 0; stage < valueof(FFT_LOG_POINTS); stage=stage+1) begin
//         stage_data[stage+1] = stage_f(fromInteger(stage), stage_data[stage]);  
//     end

//     outputFIFO.enq(stage_data[valueof(FFT_LOG_POINTS)]);
//   endrule

//   interface Put request;
//     method Action put(Vector#(FFT_POINTS, ComplexSample) x);
//         inputFIFO.enq(bitReverse(x));
//     endmethod
//   endinterface

//   interface Get response = toGet(outputFIFO);

// endmodule

// // Wrapper around The FFT module we actually want to use
// module mkFFT (FFT);
//     // FFT fft <- mkCombinationalFFT();
//     // FFT fft <- mkLinearFFT();
//     FFT fft <- mkCircularFFT();
    
//     interface Put request = fft.request;
//     interface Get response = fft.response;
// endmodule

// // Inverse FFT, based on the mkFFT module.
// // ifft[k] = fft[N-k]/N
// module mkIFFT (FFT);

//     FFT fft <- mkFFT();
//     FIFO#(Vector#(FFT_POINTS, ComplexSample)) outfifo <- mkFIFO();

//     Integer n = valueof(FFT_POINTS);
//     Integer lgn = valueof(FFT_LOG_POINTS);

//     function ComplexSample scaledown(ComplexSample x);
//         return cmplx(x.rel >> lgn, x.img >> lgn);
//     endfunction

//     rule inversify (True);
//         let x <- fft.response.get();
//         Vector#(FFT_POINTS, ComplexSample) rx = newVector;

//         for (Integer i = 0; i < n; i = i+1) begin
//             rx[i] = x[(n - i)%n];
//         end
//         outfifo.enq(map(scaledown, rx));
//     endrule

//     interface Put request = fft.request;
//     interface Get response = toGet(outfifo);

// endmodule

// module mkLinearFFT (FFT);

//     // Statically generate the twiddle factors table.
//     TwiddleTable twiddles = genTwiddles();

//     // Define the stage_f function which uses the generated twiddles.
//     function Vector#(FFT_POINTS, ComplexSample) stage_f(Bit#(TLog#(FFT_LOG_POINTS)) stage, Vector#(FFT_POINTS, ComplexSample) stage_in);
//         return stage_ft(twiddles, stage, stage_in);
//     endfunction

//     // FIFOF#(Vector#(FFT_POINTS, ComplexSample)) inputFIFO  <- mkFIFOF(); 
//     // FIFOF#(Vector#(FFT_POINTS, ComplexSample)) outputFIFO <- mkFIFOF(); 
//     // Vector#(FFT_LOG_POINTS, Reg#(Maybe#(Vector#(FFT_POINTS, ComplexSample)))) stage_data <- replicateM(mkReg(tagged Invalid));

//     // rule linear_fft;
//     //     stage_data[0] <= tagged Valid stage_f(fromInteger(0), inputFIFO.first());
//     //     inputFIFO.deq();

//     //     for(Integer stage = 0; stage < valueof(FFT_LOG_POINTS) - 1; stage = stage + 1) begin
//     //         if(isValid(stage_data[stage]))
//     //             stage_data[stage + 1] <= tagged Valid stage_f(fromInteger(stage + 1), fromMaybe(?, stage_data[stage]));  
//     //     end

//     //     if (isValid(stage_data[valueof(FFT_LOG_POINTS) - 1])) begin
//     //         outputFIFO.enq(fromMaybe(?, stage_data[valueof(FFT_LOG_POINTS) - 1]));
//     //     end
//     // endrule

//     FIFO#(Vector#(FFT_POINTS, ComplexSample)) inputFIFO <- mkFIFO();
//     FIFO#(Vector#(FFT_POINTS, ComplexSample)) outputFIFO <- mkFIFO();
//     Vector#(FFT_LOG_POINTS, FIFO#(Vector#(FFT_POINTS, ComplexSample))) stage_data <- replicateM(mkFIFO());

//     function Rules addStage(
//         Integer stage,
//         FIFO#(Vector#(FFT_POINTS, ComplexSample)) inStage,
//         FIFO#(Vector#(FFT_POINTS, ComplexSample)) outStage);

//         return (
//             rules
//                 rule oneStage;
//                     outStage.enq(stage_f(fromInteger(stage), inStage.first()));
//                     inStage.deq();
//                 endrule
//             endrules
//         );
//     endfunction

//     addRules(addStage(0, inputFIFO, stage_data[0]));
//     for (Integer stage = 0; stage < valueof(FFT_LOG_POINTS) - 1; stage = stage + 1) begin
//         addRules(addStage(stage + 1, stage_data[stage], stage_data[stage + 1]));
//     end


//     rule linear_fft;
//         outputFIFO.enq(stage_data[valueof(FFT_LOG_POINTS) - 1].first());
//         stage_data[valueof(FFT_LOG_POINTS) - 1].deq();
//     endrule

//     interface Put request;
//         method Action put(Vector#(FFT_POINTS, ComplexSample) x);
//             inputFIFO.enq(bitReverse(x));
//         endmethod
//     endinterface

//     interface Get response = toGet(outputFIFO);
// endmodule

// // 为什么mkReg里面不能是integer
// module mkCircularFFT (FFT);

//     TwiddleTable twiddles = genTwiddles();

//     function Vector#(FFT_POINTS, ComplexSample) stage_f(Bit#(TLog#(FFT_LOG_POINTS)) stage, Vector#(FFT_POINTS, ComplexSample) stage_in);
//         return stage_ft(twiddles, stage, stage_in);
//     endfunction

//     FIFO#(Vector#(FFT_POINTS, ComplexSample)) inputFIFO  <- mkFIFO(); 
//     FIFO#(Vector#(FFT_POINTS, ComplexSample)) outputFIFO <- mkFIFO();

//     Reg#(Vector#(FFT_POINTS, ComplexSample)) stage_reg <- mkReg(?);
//     Reg#(Bit#(TAdd#(TLog#(FFT_LOG_POINTS), 1))) counter <- mkReg(fromInteger(valueOf(FFT_LOG_POINTS) + 1));

//     rule stage_count if (counter < fromInteger(valueOf(FFT_LOG_POINTS)));
//         counter <= counter + 1;
//     endrule

//     rule circular_fft;
//         Vector#(FFT_POINTS, ComplexSample) stage_in;
//         Vector#(FFT_POINTS, ComplexSample) stage_out;

//         stage_in = stage_reg;
//         if (counter == 0) begin
//             stage_in = inputFIFO.first();
//             inputFIFO.deq();
//         end

//         stage_out = stage_f(truncate(counter), stage_in);

//         if (counter == fromInteger(valueOf(FFT_LOG_POINTS)) - 1) begin
//             outputFIFO.enq(stage_out);
//         end
//         else begin
//             stage_reg <= stage_out;
//         end
//     endrule

//     interface Put request;
//         method Action put(Vector#(FFT_POINTS, ComplexSample) x) if (counter == fromInteger(valueOf(FFT_LOG_POINTS)) + 1);
//             inputFIFO.enq(bitReverse(x));
//             counter <= 0;
//         endmethod
//     endinterface

//     interface Get response;
//         method ActionValue#(Vector#(FFT_POINTS, ComplexSample)) get() if (counter == fromInteger(valueOf(FFT_LOG_POINTS)));
//             counter <= counter + 1;
//             outputFIFO.deq();
//             return outputFIFO.first();
//         endmethod
//     endinterface

// endmodule