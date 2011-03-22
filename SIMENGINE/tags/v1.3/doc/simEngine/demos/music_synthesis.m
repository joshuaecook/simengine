%% Modeling Sound in simEngine - Simple Audio Synthesis

%%
%

function music_synthesis()

%%
% <html><hr></html>

%% Introduction
% This tutorial covers a simple music generator written using simEngine.  It
% requires that you have an audio card installed that works from
% within MATLAB in order to hear the music produced.  To see if your system is
% configured properly, simply try running the MATLAB script, *twinkle.m*
% from within the *examples/demos/synth* directory.
%
%  cd ([simexamplepath '/demos/synth'])
%  twinkle

%%
% The rest of this document discusses the construction of this example
% script and the underlying model found in *create_song.m*.

%%
% <html><hr></html>

%% Low-Frequency-Oscillator
% The basic component of sound synthesis is the
% Low-Frequency-Oscillator, or LFO which can be found in *create_lfo.m*.

% create_lfo - create a Low Frequency Oscillator
%
% w = frequency
% phi = phase angle
% y = output waveform, which is set to 0 if frequency is above 20kHz
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
%
function m = create_lfo(n)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n = Iterator('n', 'discrete', 'sample_frequency', 48e3);
end

% Instantiate a lfo model
m = Model('lfo');

% Create the inputs
w = m.input('w');
phi = m.input('phi', 0);

% Define the equations
t = Exp(n); % convert the iterator 'n' into a 't' that can be used in calculations
r = 2*t*pi;
y = piecewise(sin(w * r + phi), w <= 2e4 | w > 0, ...
              0);

% Create an output for y, the generated wave
m.output(y);
          
end

%%
% This model was written in a way that it can be simulated alone.  To
% see the output of this model run the following.

clear input;
input.w = 440;
o = simex(create_lfo, 0.01, input);
simplot(o);

%%
% The commands should produce a Figure plot like the one
% above.  This is a pure A4 note at 440Hz.  We have specifically cleared the variable *input* because simex
% requires that only inputs found in the model being executed are
% specified.
%
% This note can be played for one second through your sound device
% with the commands below.  The second column of the output is specified
% as the first column is the time values from simulation.  The value
% 48000 is the sampling rate that the model uses to produce output.  The
% output is scaled by 0.8 to prevent clicking.  Audio passed to sound
% must be between 1 and -1, which the LFO produces, however, values at
% the extremes will produce audible clicks.  This will be addressed later.

clear input;
input.w = 440;
o = simex(create_lfo, 1, input);
sound(0.8*o.y(:,2), 48000);

%%
% <html><hr></html>

%% Harmonics
% Real instruments don't play notes with a single tone, but actually
% produce sound as a combination of harmonics. The next model we will
% examine is one that combines several outputs from the LFO into a
% single output that comprises several superimposed harmonics.  This
% file is *create_harmonics.m*.

% create_harmonics - create a Harmonic generator from Low Frequency
% Oscillators
%
% fundamental = frequency of fundamental tone
% y = output waveform of note with harmonics superimposed

function m = create_harmonics(n)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n = Iterator('n', 'discrete', 'sample_frequency', 48e3);
end

% Instantiate a harmonics model

m = Model('harmonics');

% Create the inputs
fundamental = m.input('fundamental', 55);

% Instantiate the submodels
NUM_HARMONICS = 10;

% Fundamental tone
fundamental_tone = m.submodel(create_lfo(n));
fundamental_tone.w = fundamental;

upper_harmonics = m.submodel(create_lfo(n), 1, NUM_HARMONICS-1);
lower_harmonics = m.submodel(create_lfo(n), 1, NUM_HARMONICS-1);

multiple = 2:NUM_HARMONICS;
% Upper harmonics
upper_harmonics.w = fundamental * multiple;
% Lower harmonics
lower_harmonics.w = fundamental / multiple;

% Harmonic superposition
y = 1/2 * fundamental_tone.y;
% Add each successive harmonic pair with a lesser amplitude, such that
% the total amplitude is <= 1

for multiple = 2:NUM_HARMONICS
  y = y + ((0.5 * (1/(2^multiple))) * (upper_harmonics(multiple-1).y + lower_harmonics(multiple-1).y));
end

% Create an output for y, the generated wave
m.output(y)

end

%%
% It can be executed using the following
% commands which should produce a Figure plot that looks
% like the following.

clear input;
input.fundamental = 440;
o = simex(create_harmonics, 0.01, input);
simplot(o);

%%
% This model, like the lfo can produce an output that you can play
% directly to your sound device.  A one second sound can be played with the
% following commands.  Again the output is scaled to avoid clicking, and
% the sample rate is specified to be 48kHz.

clear input;
input.fundamental = 440;
o = simex(create_harmonics, 1, input);
sound(0.8*o.y(:,2), 48000);

%%
% <html><hr></html>

%% Chords
% Chords are comprised of multiple harmonic tones combined
% together.  A simple chord model is found in *create_chord.m* which creates a three tone
% major chord with an additional octave of the base note given a
% base note frequency.

% create_chord - create a chord synthesizer from multi-tone harmonics
%
% tonic = frequency of base note of chord
% third, fifth = half steps above base note for additional chord notes
% y = output waveform of note with chord harmonics superimposed
%
% Notes are generated in 12-TET, see create_midiscale.m for more
% information
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

function m = create_chord(n)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n = Iterator('n', 'discrete', 'sample_frequency', 48e3);
end

% Instantiate a chord model

m = Model('chord');

% Create the inputs
tonic = m.input('tonic', 110); % 110Hz = A2
third = m.input('third', 4);   % major third
fifth = m.input('fifth', 7);   % major fifth

% Instantiate the submodels
root = m.submodel(create_harmonics(n));
root.fundamental = tonic;

three = m.submodel(create_harmonics(n));
three.fundamental = (2^(1/12))^third * tonic;

five = m.submodel(create_harmonics(n));
five.fundamental = (2^(1/12))^fifth * tonic;

octave = m.submodel(create_harmonics(n));
octave.fundamental = 2*tonic;

% Harmonic superposition
y = 1/4 * (root.y + three.y + five.y + octave.y);

% Create an output for y, the generated wave
m.output(y)

end

%%
% The chord model uses
% the harmonics model to produce its tones.  This model can also
% be executed on its own with the following which will produce a plot like the one
% below.

clear input;
input.tonic = 440;
o = simex(create_chord, 0.01, input);
simplot(o);

%%
% Again, the chord can be played on the sound device with the
% commands below.

clear input;
input.tonic = 440;
o = simex(create_chord, 1, input);
sound(0.8*o.y(:,2),48000);

%%
% <html><hr></html>

%% ADSR
% When music is played on real instruments the tone is not maintained
% at a maximum loudness for the duration.  Synthesizers often model the
% varying amplitude of played notes with a component referred to as an
% ADSR, which stands for Attack, Decay, Sustain and Release.
%
% An ADSR model is included in *create_adsr.m*.

% create_adsr - create an ADSR Envelope (attack, decay, sustain, release)
%
% key = whether key is currently pressed or not, evaluated as a boolean
% attackSlope = how fast the peak is reached
% decaySlope = how fast the sustainLevel is reached after peaking
% sustainLevel = magnitude of signal after decay phase
% releaseSlope = how fast signal returns to 0 when key is released
% amplitude = output signal of signal strength
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
 
function m = create_adsr(n_in, n_out)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n_in = Iterator('n_in', 'discrete', 'sample_frequency', 64);
    n_out = Iterator('n_out', 'discrete', 'sample_frequency', 48e3);
end

% Instantiate adsr model
m = Model('adsr');

% Create the inputs
key = m.input('key', 'iter', n_in, 'halt');
attackSlope = m.input('attackSlope', 15 * n_out.sample_period);
decaySlope = m.input('decaySlope', -2 * n_out.sample_period);
sustainLevel = m.input('sustainLevel', 0.75);
releaseSlope = m.input('releaseSlope', -5 * n_out.sample_period);

% Create the states
peaked = m.state('peaked', 0, 'iter', n_out);
amplitude = m.state('amplitude', 0, 'iter', n_out);

% Define the equations
peak = 0.95;

m.recurrenceequ(peaked, piecewise(0, key == 0, ...
                                  1, amplitude >= peak, ...
                                  peaked));

attacking = piecewise(1, (key ~= 0 & amplitude < peak & peaked == 0), ...
                      0);
sustaining = piecewise(1, (key ~= 0 & peaked == 1 & amplitude <= sustainLevel), ...
                       0);
decaying = piecewise(1, (key ~= 0 & (amplitude >= peak | peaked == 1) & ~sustaining), ...
                     0);
releasing = piecewise(1, key == 0 & amplitude > 0, ...
                      0);

dy = piecewise(attackSlope, attacking, ...
               0, sustaining, ...
               releaseSlope, releasing, ...
               decaySlope, decaying, ...
               0);

m.recurrenceequ(amplitude, amplitude + dy);

m.update(amplitude, 0.01, 'when', amplitude < 0.01);
m.update(amplitude, peak, 'when', amplitude > peak);
m.update(amplitude, sustainLevel, 'when', decaying & amplitude < sustainLevel);

m.output(amplitude, 'iter', n_out);

end

%%
% Before the phases are
% discussed, run the following commands to visualize the output of the
% model.

clear input;
input.key = {[ones(1,24) zeros(1,8)]};
o = simex(create_adsr, 1, input);
simplot(o);

%%
% These commands should produce a Figure plot similar to the one
% above.  First important thing to notice in this model is the use of
% a streaming input 'key'.  This value is sampled at 64Hz and contains a
% string of ones followed by a string of zeros.  The key input is used
% as a boolean value that corresponds to whether a key on a virtual
% piano is being pressed or not.  Although we requested that the model
% run for one second, the plot only shows a half second.  This is
% because the streaming input specified in the model requires the model
% to stop executing if no more input is available.  The input provided
% for key has only 32 samples, equivalent to half a second of simulation.
%
% The ADSR has four phases which can be seen distinctly in the plot
% above.  The Attack phase is the rapid increase in amplitude up to a
% peak and is initiated when the value of key goes from zero to a
% non-zero value.  The Decay phase occurs after the output reaches its
% peak and decreases until the Sustain level is reached.  The output
% will hold the Sustain level until the key is released (returns to
% zero).  Then, the Release phase occurs which causes the output to
% decline until zero is reached (or another Attack phase is
% initiated).
%
% It is the ADSR model that we use to help smooth out the sound
% between notes to avoid choppiness and clicks that occur when
% sound reaches peak volume.  The ADSR model in 'create_adsr.m' is
% configurable and the resulting ADSR envelope can be tweaked to provide
% slightly different envelope shapes.  The shape can be controlled by
% altering the inputs to the model in the same way that scalar inputs
% were passed to models above.  For example, to change the Sustain
% level, run the following.

clear input;
input.key = {[ones(1,24) zeros(1,8)]};
input.sustainLevel = 0.5;
o = simex(create_adsr, 1, input);
simplot(o);

%%
% <html><hr></html>

%% MIDI Notes
% Thus far, the DIESEL models have been dealing with sound in
% terms of frequencies.  However, music is written in a notation
% where each note has a letter name.  While musicians rely on note
% names and graphical representations of notes, computerized audio
% has another representation in MIDI (Musical Instrument Digital
% Interface).
%
% This document will not address most of the MIDI definition,
% but will use the MIDI scale to represent notes.  In MIDI, notes
% are tuned on an equal-tempered scale where every note is an
% equal ratio of one another on a logarithmic scale.  MIDI encodes
% 128 notes from C0 through G10, nearly ten full octaves, numbered
% from 0 to 127.  The table below shows the MIDI note scale.
%
% <html>
% <table border="1" width="100%"> 
% <tr align="center"> 
% <td width="7%" rowspan="2"><b>Octave<b></td>
% <td width="93%" colspan="12"><b>MIDI Note Numbers</td> 
% </tr> 
% <tr align="center"> 
% <td><b>C</b></td> 
% <td><b>C#</b></td> 
% <td><b>D</b></td> 
% <td><b>D#</b></td> 
% <td><b>E</b></td> 
% <td><b>F</b></td> 
% <td><b>F#</b></td> 
% <td><b>G</b></td> 
% <td><b>G#</b></td> 
% <td><b>A</b></td> 
% <td><b>A#</b></td> 
% <td><b>B</b></td> 
% </tr> 
% <tr align="center"> 
% <td><b>0</b></td> 
% <td>0</td> 
% <td>1</td> 
% <td>2</td> 
% <td>3</td> 
% <td>4</td> 
% <td>5</td> 
% <td>6</td> 
% <td>7</td> 
% <td>8</td> 
% <td>9</td> 
% <td>10</td> 
% <td>11</td> 
% </tr> 
% <tr align="center"> 
% <td><b>1</b></td> 
% <td>12</td> 
% <td>13</td> 
% <td>14</td> 
% <td>15</td> 
% <td>16</td> 
% <td>17</td> 
% <td>18</td> 
% <td>19</td> 
% <td>20</td> 
% <td>21</td> 
% <td>22</td> 
% <td>23</td> 
% </tr> 
% <tr align="center"> 
% <td><b>2</b></td> 
% <td>24</td> 
% <td>25</td> 
% <td>26</td> 
% <td>27</td> 
% <td>28</td> 
% <td>29</td> 
% <td>30</td> 
% <td>31</td> 
% <td>32</td> 
% <td>33</td> 
% <td>34</td> 
% <td>35</td> 
% </tr> 
% <tr align="center"> 
% <td><b>3</b></td> 
% <td>36</td> 
% <td>37</td> 
% <td>38</td> 
% <td>39</td> 
% <td>40</td> 
% <td>41</td> 
% <td>42</td> 
% <td>43</td> 
% <td>44</td> 
% <td>45</td> 
% <td>46</td> 
% <td>47</td> 
% </tr> 
% <tr align="center"> 
% <td><b>4</b></td> 
% <td>48</td> 
% <td>49</td> 
% <td>50</td> 
% <td>51</td> 
% <td>52</td> 
% <td>53</td> 
% <td>54</td> 
% <td>55</td> 
% <td>56</td> 
% <td>57</td> 
% <td>58</td> 
% <td>59</td> 
% </tr> 
% <tr align="center"> 
% <td><b>5</b></td> 
% <td>60</td> 
% <td>61</td> 
% <td>62</td> 
% <td>63</td> 
% <td>64</td> 
% <td>65</td> 
% <td>66</td> 
% <td>67</td> 
% <td>68</td> 
% <td>69</td> 
% <td>70</td> 
% <td>71</td> 
% </tr> 
% <tr align="center"> 
% <td><b>6</b></td> 
% <td>72</td> 
% <td>73</td> 
% <td>74</td> 
% <td>75</td> 
% <td>76</td> 
% <td>77</td> 
% <td>78</td> 
% <td>79</td> 
% <td>80</td> 
% <td>81</td> 
% <td>82</td> 
% <td>83</td> 
% </tr> 
% <tr align="center"> 
% <td><b>7</b></td> 
% <td>84</td> 
% <td>85</td> 
% <td>86</td> 
% <td>87</td> 
% <td>88</td> 
% <td>89</td> 
% <td>90</td> 
% <td>91</td> 
% <td>92</td> 
% <td>93</td> 
% <td>94</td> 
% <td>95</td> 
% </tr> 
% <tr align="center"> 
% <td><b>8</b></td> 
% <td>96</td> 
% <td>97</td> 
% <td>98</td> 
% <td>99</td> 
% <td>100</td> 
% <td>101</td> 
% <td>102</td> 
% <td>103</td> 
% <td>104</td> 
% <td>105</td> 
% <td>106</td> 
% <td>107</td> 
% </tr> 
% <tr align="center"> 
% <td><b>9</b></td> 
% <td>108</td> 
% <td>109</td> 
% <td>110</td> 
% <td>111</td> 
% <td>112</td> 
% <td>113</td> 
% <td>114</td> 
% <td>115</td> 
% <td>116</td> 
% <td>117</td> 
% <td>118</td> 
% <td>119</td> 
% </tr> 
% <tr align="center"> 
% <td><b>10</b></td> 
% <td>120</td> 
% <td>121</td> 
% <td>222</td> 
% <td>123</td> 
% <td>124</td> 
% <td>125</td> 
% <td>126</td> 
% <td>127</td> 
% <td colspan="4">&nbsp;</td> 
% </tr> 
% </table>
% </html>
%
% The frequencies of the notes are determined based on the
% formula for a 12 part octave (12-TET) given in *create_midiscale.m* with the frequency
% of A4 set to 440Hz.  This model takes a note number and converts
% it to a frequency.  This model starts with E0, the first note
% above 20Hz and ends with D#10 the last note below 20kHz. Notes 0
% through 3 or values above 123 all produce a frequency of 0, no
% sound.

% create_midiscale - create a MIDI scale converter
%
% note = MIDI note number
% frequency = output frequency of the corresponding MIDI note number
%
% Produces a note frequency for a note in the MIDI scale based on A4 (the 57th MIDI note) set to 440 Hz
% Valid notes are in the range 5 to 123 (20Hz to 19.9kHz base frequencies)
% The MIDI scale is 12-TET meaning the octave is broken into 12 equistant parts on a logarithmic scale
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

function m = create_midiscale(n)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n = Iterator('n', 'discrete', 'sample_frequency', 1);
end

% Instantiate a midiscale model
m = Model('midiscale');

% Create the inputs
note = m.input('note', 'iter', n, 'halt');

% Equations
frequency = piecewise(440 * (2^(1/12))^(note - 57), note > 3 & note < 124, 0);

% Create an output frequency
m.output(frequency)

end

%%
% This model only produces the frequency values for notes and
% therefore can't be run by itself to produce audio, but you can
% run it to see what the frequencies are for the MIDI scale.  To
% do this, run the following.

clear input;
input.note = {[0:131]};
o = simex(create_midiscale, 131, input);
reshape(o.frequency(:,2),11,12)'

%%
% This code checks the note numbers 0
% through 131 and displays them in the same orientation as the
% table above.  Notice how the frequncy value for the first and
% last notes are 0, and only frequencies between 20Hz and 20kHz
% are produced.


%%
% <html><hr></html>

%% Putting it all together
% The final model in this series is the model found in *create_song.m*.  This model combines the
% above models to create a translator that takes a stream of notes
% and chords in MIDI note encoding and produces an audio music stream
% that can be played through the sound device.

% create_song - create a song model that will synthesize music
% based on an input stream of MIDI notes
%
% notes = streaming input of MIDI notes
% chords = streaming input of MIDI chord base notes
% music = mono stream of 48kHz PCM values that can be played directly to
%         a sound card
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

function m = create_song()

% Instantiate a song model
m = Model ('song');

n_in = Iterator('n_in', 'discrete', 'sample_frequency', 64);
n_out = Iterator('n_out', 'discrete', 'sample_frequency', 48e3);

notes = m.input('notes', 'iter', n_in, 'halt');
chords = m.input('chords', 'iter', n_in, 'halt');

% Hold the last note/chord when released, ADSR does fade out
last_note = m.state('last_note', 0, 'iter', n_in);
last_chord = m.state('last_chord', 0, 'iter', n_in);

m.recurrenceequ(last_note, piecewise(notes, notes > 0, last_note))
m.recurrenceequ(last_chord, piecewise(chords, chords > 0, last_chord))

% Produce the freqencies of the base notes for the MIDI notes and chords
ms_note = m.submodel(create_midiscale(n_in));
ms_note.note = last_note;
ms_chord = m.submodel(create_midiscale(n_in));
ms_chord.note = last_chord;

% Produce the full harmonics for notes and chords
melody = m.submodel(create_harmonics(n_out));
melody.fundamental = ms_note.frequency;

the_chords = m.submodel(create_chord(n_out));
the_chords.tonic = ms_chord.frequency;

% Create separate ADSR envelopes for notes and chords
adsr_note = m.submodel(create_adsr(n_in, n_out));
adsr_note.key = notes;

adsr_chord = m.submodel(create_adsr(n_in, n_out));
adsr_chord.key = chords;

% Combine notes and chords into a single chanel mono output
music = 0.5 * ((melody.y * adsr_note.amplitude) + ...
               (the_chords.y * adsr_chord.amplitude));

% Create the output for the synthesized PCM waveform data
m.output(music, 'iter', n_out);

end

%%
% This model has two input channels, one for melody notes, and one
% for chords that are each run through a *create_midiscale.m*
% model.  The melody is played with single notes using the
% *create_harmonics.m* model, while the chords are created with
% the *create_chord.m* model.  Both channels are passed through
% separate *create_adsr.m* instances to smooth out the sound
% before being combined together to produce the final output.
%
% One trick that we employ is to use the *notes* and
% *chords* inputs to both generate the frequencies as well as
% trigger the ADSR.  To make this happen smoothly, we add two
% state variables that hold the last non-zero value for notes and chords.
% These states are passed to the *create_midiscale.m* instances
% instead of the input directly so that a note will continue to
% sound during the release phase of the ADSR.  A zero in the input
% causes the ADSR to switch to the release phase, which must occur
% before another attack phase (another note or chord) is
% possible.
%
% Inside the MATLAB script, *twinkle.m* you can see that
% each note (quarter, half and whole) is comprised of two phases
% built with the *ones()* and *zeros()* functions.  The
% length of the overall string of ones and zeros determines the
% length of the note, and the relative lengths between ones and
% zeros controls the shape of the ADSR based on when the release
% phase is triggered.
%
% The value for *n* will set the key for the song, and the
% phrases are built relative to this value.  To change the speed
% at which the song is played, you can either change the
% definition of the notes themselves, or change the sampling rate
% in *create_song.m* from 64 samples per second, to another
% value.

% Sample driver for create_song.m that plays 'Twinkle, twinkle, little star'
% Copyright (C) 2010 Simatra Modeling Technologies

function twinkle()
% The song model samples inputs at 64Hz, the definitions provided
% below scale a quarter note such that there are 120 beats per
% minute. The ones() denote that the note is actively being
% sounded, the zeros() indicate that the note should be allowed to decay.
q = [ones(1,24) zeros(1,8)]; % Quarter note
h = [ones(1,56) zeros(1,8)]; % Half note
w = [ones(1,120) zeros(1,8)]; % Whole note

% Base note to set the Key.  The value 50 is the MIDI note for D4.
note = 50;
% Base note for the chords.  Play the chords an octave lower (12 half steps).
chord = note - 12;

% 'Twinkle, twinkle ...' melody phrase of the song
twinkle = [note*q note*q (note+7)*q (note+7)*q ...
           (note+9)*q (note+9)*q (note+7)*h ...
           (note+5)*q (note+5)*q (note+4)*q (note+4)*q ...
           (note+2)*q (note+2)*q note*h];

% 'Up a-bove the world so high, ...' melody phrase
diamond = [(note+7)*q (note+7)*q (note+5)*q (note+5)*q ...
           (note+4)*q (note+4)*q (note+2)*h ...
           (note+7)*q (note+7)*q (note+5)*q (note+5)*q ...
           (note+4)*q (note+4)*q (note+2)*h];

% The same two phrases above but the chords
chord_twinkle = [chord*w ...
                 (chord+5)*h chord*h ...
                 (chord+5)*h chord*h ...
                 (chord+7)*h chord*h];

chord_diamond = [chord*h (chord+5)*h ...
                 chord*h (chord+7)*h ...
                 chord*h (chord+5)*h ...
                 chord*h (chord+7)*h];

% The full song repeats the first phrase
i.notes = {[twinkle diamond twinkle]};
i.chords = {[chord_twinkle chord_diamond chord_twinkle]};

% Run the notes and chords through the song model. The time 24
% must be greater than or equal to the length of the song to prevent
% truncation. The song will stop automatically when the notes or
% chords inputs run out.
o = simex(create_song, 24, i);

% Play the resulting audio through the sound device, grabbing only
% the second column of the output (first column is the time values
% from simulation).  The resultant audio is sampled at 48kHz and
% should be played with the same frequency to produce accurate tones.
sound(o.music(:,2), 48000);

end


%%
% <html><hr></html>

%% Try for yourself
% 1. Create another song to play through the model by encoding the
% notes on the MIDI scale.
%%
% 2. Alter the ADSR parameters to get a different sound.  The
% unspecified default inputs can be included in the top level
% model so that they can be set from within MATLAB.
%%
% 3. Change the model to take a stereo set of inputs and produce a
% stereo output.  Stereo audio is played in MATLAB with the same
% sound command which only requires a two column matrix, one for
% each of the left and right channel.

%%
% <html><hr></html>

end