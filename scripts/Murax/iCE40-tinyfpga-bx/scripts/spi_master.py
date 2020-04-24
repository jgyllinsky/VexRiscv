# Python3

from pyftdi.spi import SpiController # , SpiIOError

ctrl = SpiController()

ctrl.configure('ftdi://ftdi:232h/1')
# spi = ctrl.get_port(cs=0, freq=20e6, mode=1)  # maximum
spi = ctrl.get_port(cs=0, freq=10e3, mode=1)

# spi.exchange(out=b'1234')
spi.exchange(out=[0x12, 0x34])
spi.exchange(out=[0x87, 0x65])
spi.exchange(out=[0x55, 0xaa])
spi.exchange(out=[0xaa, 0x55])
