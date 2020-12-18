import miditoolkit
from chorder import Chord, Dechorder

path_midi = "../cherrylips.mid"

def uniq(lst):
  aux = []
  for x in lst:
    if x not in aux:
      aux.append(x)
  return aux

midi_obj = miditoolkit.midi.parser.MidiFile(path_midi)
chords = Dechorder.dechord(midi_obj, scale=None)
chrds = uniq(map(str, filter(lambda chord: chord.root_pc, chords)))
# print(len(chrds))
print(chrds)

