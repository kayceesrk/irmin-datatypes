# irmin-datatypes
A collection of functional, mergeable datatypes for Irmin. Currently supports:

 * **LWW register** -- A last-writer-wins register based on write timestamps.
 * **Queue** -- An efficient double-ended queue. Uses liked representation on
	 the append-only store. Provides O(1) push and pop operations.
 * **Set** -- A "naive" set implementation. Serializes the set into a single
	 blob.
