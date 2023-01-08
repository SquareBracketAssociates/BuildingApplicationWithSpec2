## Layouts 
@cha_layout


Hi. In Spec1, the UI layout was described in the class side defaultSpec method. In the TODO tutorial, the layout is given is the initializePresenters instance side method. What is the prefered way in Spec2 ? Stick with class side method, or add it in initializePresenters ?
Esteban Lorenzano
 —
10/27/2020
in spec2 layout will be created in instance side and class-side accessors will remain for those who prefer it.
this is to reflect the dynamic nature of layouts in spec2, and the fact that you can compose them using directly presenter instances, not forcing you to declare them by name before.
now... it is possible that there are cases where you want the layout "template" instead the layout instantiated... so you still can do it.

### Basic principle reminder


To define the layout of a presenter you can: 
- Define the `defaultLayout` on the instance side.
- Use `layout:` in your `initlizePresenters` method

Both layout methods should return a layout for example instance of `SpBoxLayout` or `SpPanedLayout`.

### How can we get normal button

```
WindowExamplePresenter >> initializePresenters

  button1 := self newButton.
  button2 := self newButton.
  button1 label: '+'.
  button2 label: '-'.
  
WindowExamplePresenter class >> defaultSpec
 	^ SpBoxLayout newLeftToRight
	add: #button1; 
	add: #button2; yourself
```


This produces a large window....



### BoxLayout (SpBoxLayout and SpBoxConstraints)

It show presenters in an ordered box. Box can be horizontal or vertical and
presenters will be ordered top to down or left to right following direction decided.
The basic message to add presenters is: #add:expand:fill:padding:
expand   - true if the new child is to be given extra space allocated to box .
     The extra space will be divided evenly between all children that use this option
fill   - true if space given to child by the expand option is actually allocated to child ,
     rather than just padding it. This parameter has no effect if expand is set to false.
padding  - extra space in pixels to put between this child and its neighbors, over and above
     the global amount specified by “spacing” property. If child is a widget at one of
     the reference ends of box , then padding pixels are also put between child and the
     reference edge of box"

```smalltalk
SpBoxLayout newVertical  spacing: 15;
 add: #button1 expand: false fill: true padding: 5;
 add: #button2 withConstraints: [ :constraints | constraints width: 30; padding: 5];
 addLast: #button3 expand: false fill: true padding: 5;
 yourself
```

Element in a vertical box will use all available horizontal space, and fill
vertical space according to the rules. This will be inverted with horizontal box.

Box layout can be composed, we can add a box to an existing one.

### GridLayout (SpGridLayout, SpGridConstraints and SpGridAxisConstraints)

I can arrange submorphs in a grid according to its properties (position and
span, see GridLayoutProperties), and according certain layout properties:
I can place my elements in a grid, following some constraints:

- a position is mandatory (columnNumber@rowNumber)
- a span can be added if desired (columnExtension@rowExtention)

- columnHomogeneous -> weather a columns will have same size.
- rowHomogeneous -> weather a row will have same size.
- padding -> the padding to start drawing cells ??? => to be confirmed
- colSpacing -> the column space between cells
- rowSpacing -> the row space between cells

```smalltalk
SpGridLayout new
  add: 'Name:' at: 1@1;
  add: #nameText  at: 2@1;
  add: 'Password:' at: 1@2;
  add: #passwordText at: 2@2;  
  add: #acceptButton at: 1@3;
  add: #cancelButton at: 2@3 span: 2@3;
  add: 'test label' at: 1@4;
  yourself
```  

As of this writing (january 13, 2020), we cannot add a box layout to a grid.

### Paned layout (SpPanedLayout and SpPanedConstraints)

A paned layout is like a Box Layout (it places childen in vertical or horizontal
fashion), but it will add a splitter in between, that user can drag to resize the panel.
In exchange, a paned layout can have just two children. Position Indicates
original position of splitter. It can be nil (then it defaults to 50%) or It can
be a percentage (e.g. 30 percent)

```smalltalk
SpPanedLayout newHorizontal position: 80 percent;
                        add: '#acceptButton';
                        add: #cancelButton; yourself.
```

		
		
### Overlay

```
app := SpApplication new.
app addStyleSheetFromString: '.application [
		.green [ 
			Draw {
				#backgroundColor: #16A085
			}
		],
		.redOverlay [
			Draw { #backgroundColor: #C0392BBB },
			Geometry { #height: 150, #width: 150 }
		],
		.title [ Font { #size: 40, #bold: true },
			Geometry { #height: Reset, #width: Reset } ]
]'.

presenter := SpPresenter newApplication: app.
	
child := presenter newPresenter
 	layout: (SpBoxLayout new
 		hAlignCenter;
 		vAlignCenter;
		add: ('I AM THE CHILD' asPresenter
  				addStyle: 'title';
				yourself);
			yourself);
  		addStyle: 'green';
		yourself.
		
overlay := presenter newPresenter
	layout: SpBoxLayout newVertical;
	addStyle: 'redOverlay';
	yourself.

presenter layout: (SpOverlayLayout new
	 child: child;
	 addOverlay: overlay withConstraints: [ :c | 
		 c
			 vAlignCenter;
			 hAlignCenter ];
	 yourself).
			
presenter open.

```