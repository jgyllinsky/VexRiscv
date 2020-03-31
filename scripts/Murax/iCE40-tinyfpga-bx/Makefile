
VBASE = ../../..
VNAME = Murax_iCE40_tinyfpga_bx
VERILOG = ${VBASE}/${VNAME}.v

all: prog

${VERILOG} :
	(cd ${VBASE}; sbt "runMain flogics.vexriscv.tinyfpga.${VNAME}")

generate : ${VERILOG}

${VERILOG}*.bin:

# bin/${VNAME}.blif : ${VERILOG} ${VERILOG}*.bin
bin/${VNAME}.json : ${VERILOG} ${VERILOG}*.bin
	mkdir -p bin
	rm -f ${VNAME}.v*.bin
	cp ${VERILOG}*.bin . | true
	# yosys -v3 -p "synth_ice40 -top ${VNAME} -blif bin/${VNAME}.blif" ${VERILOG}
	yosys -v3 -p "synth_ice40 -top ${VNAME} -json bin/${VNAME}.json" ${VERILOG}

# bin/${VNAME}.asc : ${VNAME}.pcf bin/${VNAME}.blif
# 	arachne-pnr -p ${VNAME}.pcf -d 8k --max-passes 600 -P cm81 bin/${VNAME}.blif -o bin/${VNAME}.asc

bin/${VNAME}.asc : ${VNAME}.pcf bin/${VNAME}.json
	nextpnr-ice40 --pcf ${VNAME}.pcf --lp8k --package cm81 --json bin/${VNAME}.json --asc bin/${VNAME}.asc

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
