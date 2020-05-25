
VBASE = ../../../..
VNAME = Briey_iCE40_tinyfpga_bx
VERILOG = ${VBASE}/${VNAME}.v

all: prog

${VERILOG} :
	(cd ${VBASE}; sbt "runMain flogics.vexriscv.tinyfpga.${VNAME}")

generate : ${VERILOG}

${VERILOG}*.bin:

bin/${VNAME}.json : ${VERILOG}
	mkdir -p bin
	yosys -v3 -p "synth_ice40 -top ${VNAME} -json bin/${VNAME}.json" ${VERILOG}

bin/${VNAME}.asc : ${VNAME}.pcf bin/${VNAME}.json
	nextpnr-ice40 --freq 16 --pcf ${VNAME}.pcf --lp8k --package cm81 --json bin/${VNAME}.json --asc bin/${VNAME}.asc --log ${VNAME}_nextpnr.log

bin/${VNAME}.bin : bin/${VNAME}.asc
	icepack bin/${VNAME}.asc bin/${VNAME}.bin

compile : bin/${VNAME}.bin

time: bin/${VNAME}.bin
	icetime -tmd lp8k bin/${VNAME}.asc

prog : bin/${VNAME}.bin
	tinyprog -p $^

sudo-prog : bin/${VNAME}.bin
	sudo tinyprog -p $^

clean :
	rm -rf bin
	rm -f ${VNAME}.v*.bin
	rm -f ${VERILOG}*.bin
	rm -f ${VERILOG}
