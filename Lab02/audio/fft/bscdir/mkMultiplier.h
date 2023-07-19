/*
 * Generated by Bluespec Compiler (build 14ff62d)
 * 
 * On Thu Jul 13 05:06:53 CST 2023
 * 
 */

/* Generation options: keep-fires */
#ifndef __mkMultiplier_h__
#define __mkMultiplier_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkMultiplier module */
class MOD_mkMultiplier : public Module {
 
 /* Clock handles */
 private:
  tClock __clk_handle_0;
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_Fifo<tUInt32> INST_results;
 
 /* Constructor */
 public:
  MOD_mkMultiplier(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUInt8 PORT_EN_putOperands;
  tUInt8 PORT_EN_getResult;
  tUInt32 PORT_putOperands_coeff;
  tUInt32 PORT_putOperands_samp;
  tUInt8 PORT_RDY_putOperands;
  tUInt32 PORT_getResult;
  tUInt8 PORT_RDY_getResult;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_WILL_FIRE_getResult;
  tUInt8 DEF_WILL_FIRE_putOperands;
  tUInt8 DEF_CAN_FIRE_getResult;
  tUInt8 DEF_CAN_FIRE_putOperands;
 
 /* Local definitions */
 private:
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  void METH_putOperands(tUInt32 ARG_putOperands_coeff, tUInt32 ARG_putOperands_samp);
  tUInt8 METH_RDY_putOperands();
  tUInt32 METH_getResult();
  tUInt8 METH_RDY_getResult();
 
 /* Reset routines */
 public:
  void reset_RST_N(tUInt8 ARG_rst_in);
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
  void set_clk_0(char const *s);
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkMultiplier &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkMultiplier &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkMultiplier &backing);
};

#endif /* ifndef __mkMultiplier_h__ */
