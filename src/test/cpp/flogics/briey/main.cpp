#include "VBriey.h"
#include "VBriey_MiniBriey.h"
//#include "VBriey_Axi4VgaCtrl.h"
//#include "VBriey_VgaCtrl.h"
#ifdef REF
#include "VBriey_RiscvCore.h"
#endif
#include "verilated.h"
#include "verilated_vcd_c.h"
#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <stdint.h>
#include <cstring>
#include <string.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <iomanip>
#include <time.h>
#include <unistd.h>

#include "VBriey_VexRiscv.h"


#include "framework.h"
#include "../../common/jtag.h"
#include "uart.h"



class VexRiscvTracer : public SimElement{
public:
	VBriey_VexRiscv *cpu;
	ofstream instructionTraces;
	ofstream regTraces;

	VexRiscvTracer(VBriey_VexRiscv *cpu){
		this->cpu = cpu;
#ifdef TRACE_INSTRUCTION
	instructionTraces.open ("instructionTrace.log");
#endif
#ifdef TRACE_REG
	regTraces.open ("regTraces.log");
#endif
	}



	virtual void preCycle(){
#ifdef TRACE_INSTRUCTION
		if(cpu->writeBack_arbitration_isFiring){
			instructionTraces <<  hex << setw(8) <<  cpu->writeBack_INSTRUCTION << endl;
		}
#endif
#ifdef TRACE_REG
		if(cpu->writeBack_RegFilePlugin_regFileWrite_valid == 1 && cpu->writeBack_RegFilePlugin_regFileWrite_payload_address != 0){
			regTraces << " PC " << hex << setw(8) <<  cpu->writeBack_PC << " : reg[" << dec << setw(2) << (uint32_t)cpu->writeBack_RegFilePlugin_regFileWrite_payload_address << "] = " << hex << setw(8) << cpu->writeBack_RegFilePlugin_regFileWrite_payload_data << endl;
		}

#endif
	}
};




#include <SDL2/SDL.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>


class Display : public SimElement{
public:
	int width, height;
	uint32_t *pixels;
    SDL_Window* window;
    SDL_Renderer* renderer;
    SDL_Texture * texture;
    uint32_t x,y;
    uint32_t refreshCounter = 0;

    Display(int width, int height){
		this->width = width;
		this->height = height;
		x = y = 0;
		init();
	}

	virtual ~Display(){
	    delete[] pixels;
	    SDL_DestroyTexture(texture);
	    SDL_DestroyRenderer(renderer);
	    SDL_DestroyWindow(window);
	    SDL_Quit();
	}

	void init(){

        /* Initialize SDL. */
        if (SDL_Init(SDL_INIT_VIDEO) < 0)
                return;

        /* Create the window where we will draw. */
        window = SDL_CreateWindow("VGA",
                        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                        width, height,
                        SDL_WINDOW_SHOWN);

        /* We must call SDL_CreateRenderer in order for draw calls to affect this window. */
        renderer = SDL_CreateRenderer(window, -1, 0);

        texture = SDL_CreateTexture(renderer,
            SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STATIC, width, height);
        pixels = new Uint32[width * height];
        memset(pixels, 0, width * height * sizeof(Uint32));
	}

	void set(uint32_t color){
		pixels[x + y*width] = color;
	}

	void incX(){
		x++;
		if(x >= width) x = width;
	}

	void incY(){
		y++;
		if(y >= height) y = height;
	}

	void refresh(){
		//cout << "Display refresh " << refreshCounter++ << endl;
		SDL_UpdateTexture(texture, NULL, pixels, 640 * sizeof(Uint32));
		SDL_RenderClear(renderer);
		SDL_RenderCopy(renderer, texture, NULL, NULL);
		SDL_RenderPresent(renderer);
        memset(pixels, 0, width * height * sizeof(Uint32));
	}

	virtual void postCycle(){

	}

	virtual void preCycle(){

	}
};

class BrieyWorkspace : public Workspace<VBriey>{
public:
	BrieyWorkspace() : Workspace("Briey"){
		ClockDomain *axiClk = new ClockDomain(&top->io_axiClk,NULL,20000,100000);
		AsyncReset *asyncReset = new AsyncReset(&top->io_asyncReset,50000);
		Jtag *jtag = new Jtag(&top->io_jtag_tms,&top->io_jtag_tdi,&top->io_jtag_tdo,&top->io_jtag_tck,80000);
		UartRx *uartRx = new UartRx(&top->io_uart_txd,1.0e12/115200);
		timeProcesses.push_back(axiClk);
		timeProcesses.push_back(asyncReset);
		timeProcesses.push_back(jtag);
		timeProcesses.push_back(uartRx);

		axiClk->add(new VexRiscvTracer(top->MiniBriey->axi_core_cpu));
	}
};


#if 0
struct timespec timer_start(){
    struct timespec start_time;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);
    return start_time;
}

long timer_end(struct timespec start_time){
    struct timespec end_time;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_time);
    uint64_t diffInNanos = end_time.tv_sec*1e9 + end_time.tv_nsec -  start_time.tv_sec*1e9 - start_time.tv_nsec;
    return diffInNanos;
}
#endif


int main(int argc, char **argv, char **env) {

	Verilated::randReset(2);
	Verilated::commandArgs(argc, argv);

	printf("BOOT\n");
	// timespec startedAt = timer_start();

	BrieyWorkspace().run(1e9);

	// uint64_t duration = timer_end(startedAt);
#if 0
	cout << endl << "****************************************************************" << endl;
	cout << "Had simulate " << workspaceCycles << " clock cycles in " << duration*1e-9 << " s (" << workspaceCycles / (duration*1e-9) << " Khz)" << endl;
	cout << "****************************************************************" << endl << endl;
#endif


	exit(0);
}
