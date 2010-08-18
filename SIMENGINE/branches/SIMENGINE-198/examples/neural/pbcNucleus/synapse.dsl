model (isynapse) = synapse(Spre, Vpost)
	input Spre with {default = 0}
	input Vpost with {default = 0}

	constant gsyn = 0.3
	constant Esyn = 0
	
	equation isynapse = 0.3*Spre*(Vpost-Esyn)
end
