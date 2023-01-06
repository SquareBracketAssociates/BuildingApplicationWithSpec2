## Layouts 
@cha_layout


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


### Boxed


### Paned


### Grid

		
		
		
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