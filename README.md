# irmin-datatypes
A collection of functional, mergeable datatypes for Irmin. Currently supports:

	* LWW register -- A last-writer-wins register based on write-timestamps.
		Polymorphic over the type of values stored in the register.
