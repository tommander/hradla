# Hradla

This is a small project for playing around with logic circuits.

At this moment, it is not even in an alpha version and some commits may be totally broken. A lot of things still to be done.

What it can do right now is combine four elementary components (wire, logic gate, data input, data output) into bigger components that can be saved as files and these can all be combined into a circuit (internally called "map") with a "simulation" functionality. It has very detailed logging to inspect the inner workings.

More information will follow soon :)

## TODO

- Currently selected (i.e. last clicked on) component should be highlighted with e.g. blue color
- Group together components to move together and make a bigger "circuit" easier to navigate
- Scrollbox around the whole main TImage
  - Viewbox
  - Customizable field size
- Component list should be shown as (small?) icons
- Folder structure in custom components
  - Trash folder?
- Running inputstring should have the first tick immediate

## Later

- Complete redesign
- Gate (propagation) delay
- Power consumption
- Blackbox components (the "inner" circuit of such component is hidden and the component can therefore be smaller on the circuit, making it way easier to implemented complex circuits)
- Programmable components / "make-your-own-assembler"