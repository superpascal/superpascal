# ZealPascal
A Self hosted Pascal for ZealOS on the Z80 Microprocessor

## About the Zeal 8-bit Computer

The Zeal 8-bit Computer is a uniquely powerful Z80-based system that stands apart from classic 8-bit architectures through its innovative hardware design.

### Hardware Specifications

- **CPU**: Zilog Z80 running at 10 MHz
- **Memory**: 512 KB SRAM
- **Memory Management**: 16 KB paging system for efficient memory access
- **Video Subsystem**: FPGA-based video processing unit

### FPGA-Based Video Subsystem

The most significant architectural advantage of the Zeal 8-bit Computer is its FPGA-based video subsystem. This dedicated hardware component completely offloads all graphics processing from the Z80 CPU, including:

- **Rendering**: All screen drawing operations are handled by the FPGA
- **Sprite Handling**: Hardware sprite management and composition
- **Tilemaps**: Built-in tilemap rendering support
- **Timing**: Video timing and synchronization
- **Raster Effects**: Hardware-accelerated raster effects
- **Screen Refresh**: Automatic screen refresh without CPU intervention

This architecture means the Z80 CPU is free to focus entirely on program logic, game mechanics, and application processing, rather than being burdened with video timing and rendering tasks.

### Comparison with Classic Z80 Machines

Zeal is fundamentally different from classic Z80-based computers:

**Classic Z80 Systems** (ZX Spectrum, Amstrad CPC, MSX, TRS-80, CP/M systems):
- CPU must directly manage video memory
- CPU is responsible for screen refresh timing
- Significant CPU cycles consumed by video handling
- Limited or no hardware sprite support
- Raster effects require precise CPU timing

**Zeal 8-bit Computer**:
- FPGA handles all video operations independently
- Z80 CPU is free from video timing constraints
- Hardware-accelerated graphics operations
- Dedicated sprite and tilemap engines
- Enables more complex applications and games with smooth performance

This architectural approach makes the Zeal 8-bit Computer uniquely capable among 8-bit systems, offering performance characteristics that were previously impossible on traditional Z80 platforms.
