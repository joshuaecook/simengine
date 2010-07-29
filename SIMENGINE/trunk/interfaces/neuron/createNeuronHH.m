function n = createNeuronHH

n = Neuron('my_hh_neuron');

soma = n.section('soma');
soma.insert('hh');

% Create a current injection stimulus
stim = n.IClamp(soma, 0.5);
stim.del = 25;
stim.dur = 50;
stim.amp = 10;

end
