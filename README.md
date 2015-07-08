# Tomasulos-Algorithm-with-Speculation
Tomasulo’s with Speculation created with <a href="https://github.com/chrisban">Christopher Ban</a> and <a href="https://github.com/funkeyfreak">Dalin Williams</a>
<h2>Regfile</h2>
The regfile records the current values of each register. Once an instruction’s result has been committed the result will be updated in the regfile.

<h2>ROB</h2>
The reorder buffer is a circular holding and reordering station/buffer for instructions issued from the issuer. Once an instruction has been entered via the issuer, the ROB will seek a value for its registers and register t entries, and returns the index of the entered instruction if it does not exist, or the value from the CDB if it does. The ROB will not accept any incoming instructions if it is full. This is determined via the head and tail pointers of the buffer. If these are equal, the ROB is full. Finally, a ROB entry will update as the CDB broadcasts the value of said entries r or t entries as well as the computed result of the instruction. Once an instruction has completed, it is marked as done via the ready bit. Finally, an instruction in the CDB will not commit unless it is in order. This is to say that the entry which will commit will be the oldest in the ROB. Once this instruction commits, the ROB entry is committed to the register file, and the ROB entry is cleared.

<h2>Reservation Station</h2>
The reservation station stores and holds the instructions issued by the issuer. Once instructions are issued and the ROB is not full, an entry is placed is the reservation station. This entry may have a ROB entry in the data local for the data of the s and t registers. If there is a ROB entry, then the entry must wait until the value is flashed across the CDB. This CDB flash must contain the address of the entries corresponding s or t registers. If it does, the entry is updated. If the entry had all valid data for its instruction registers, the entry is then placed within a pipeline for execution in the respective ALU unit. The pipeline serves as a secondary staging ground for completed instructions. This staging ground is only for complete and ready to run instructions. Instructions within the pipeline have a timer which, when equal to zero, with proceed to check if the cost is clear to run an operation on an ALU/logical unit. If the logical unit is not full, the pipeline entry is emptied into its respective unit and computed. Upon the data being transmitted across the CDB, the pipelining segment holding this value will then delete it from the pipeline.

<h2>CDB</h2>
The CDB is the broadcasting component of Tomasulo’s Algorithm. The CDB is an interconnected bus that allows all components of the component to communicate. This is achieved via the broadcasting of results across the CDB. Whenever an operation unit completes an operation, the CDB waits a cycle, then broadcasts the data. However, the CDB must also handle multiple broadcasts in parallel, as this would cause a conflict on write, and data to be lost.

<h2>Issuer</h2>
Issues instructions to the relevant unit depending on instruction and ROB status.

<h2>Memory Unit</h2>
The memory unit acts as the bridge between the load/store instructions and memory. In this component we handle when a load instruction is pulling from memory and when a store instruction is pushing to memory. We simulate the interaction this component would have with the instructions and memory.

<h2>Address Unit</h2>
The address unit is used to calculate the address for load words.

<h3>Other Modules and Components</h3>
Please note that we include the semi-related ALU, FP Adders/Mults/etc. components along with the aforementioned components in the resources provided.
