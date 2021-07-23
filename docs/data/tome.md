# ATtiny814

* Document [Datasheet](https://ww1.microchip.com/downloads/en/DeviceDoc/ATtiny417-814-816-817-DataSheet-DS40002288A.pdf)

## Overview
## Topic Clock Rate

The ATtiny boots up with the 16/20 MHz oscillator (OSC20M) running at 20 MHz as the clock source and a 
prescaler division factor of 6 giving a frequency of at 3.333 MHz <ref page="80"/>.
The clock source and clock prescaler can be configured by temporarily disabling I/O register 
Configuration Change Protection (CCP). This is done by setting 0xD8 as the value of 
<reg>CPU.CCP.CCP</reg> register, followed by setting the desired values to <reg>CLKCTRL.MCLKCTRLA.CLKSEL</reg>
and <reg>CLKCTRL.MCLKCTRLB.PDIV</reg> registers. <topic>Fuses</topic> define whether OSC20M runs at 16 
or 20 MHz: <reg>FUSE.OSCCFG.FREQSEL</reg>. 

Ex 1: Disable prescaler to run at full 16/20 MHz

```
#include <avr/io.h>
CPU_CCP = CCP_IOREG_gc;
CLKCTRL_MCLKCTRLB = 0;
```

## Module SPI
The Serial Peripheral Interface is very straightforward to use in regular polling mode without messing around with the extra buffers that are available.
### Master setup
Setup involves setting pin directions, configuring <reg>SPI.CTRLA</reg> and <reg>SPI.CTRLB</reg>.
```avr
// Configure your chip select pins as outputs and set them high
// MOSI and SCK must be set as outputs
PORTA_DIRSET = PIN1_bm | PIN3_bm;
// Slave Select Disable - enable only for masters operating in multimaster environments
SPI0_CTRLB = SPI_SSD_bm;
// Operate as master, set clock divider to 128 and enable SPI controller
SPI0_CTRLA = SPI_MASTER_bm | SPI_PRESC_DIV128_gc | SPI_ENABLE_bm;
```
### Transreceiving data by polling
Transreceiving data is achieved by writing into <reg>SPI.DATA</reg>, waiting for an interrupt in <reg>SPI.INTFLAGS.RXCIF/IF</reg> and finally reading the result from the DATA register.
```avr
uint8_t spi_transfer(uint8_t data) {
    // Initiate transfer by writing data
    SPI0_DATA = data;
    // Wait for interrupt indicating transreceiving is completed
    while (!(SPI0_INTFLAGS & SPI_IF_bm)) {}
    // Read received data
    data = SPI0_DATA;
    return data;
}
```

# ATtiny202 -> ATtiny814
