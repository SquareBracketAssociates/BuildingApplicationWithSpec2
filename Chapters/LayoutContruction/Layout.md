## Layouts
@cha_layout


In Spec2 layouts are represented by instances of layout classes. Such layout classes encode different positioning of elements such as box, paned, or grid layouts.
This chapter presents the existing layouts, their definition and how layouts can be reused when 
a presenter reuses other presenters.

### Basic principle reminder

Spec expects to get layouts objects, instances of the layout classes, associated with a presenter. Each presenter should describe the positioning of its sub presenters. 

Contrary to Spec1.0 where layouts were only defined at the class level, in Spec2.0
to define the layout of a presenter you can: 
- Define the `defaultLayout` method on the instance side,
- Or use the message `layout:` in your `initializePresenters` method to set an instance of layout in the current presenter.

Both layout methods should return a layout, for example, instance of `SpBoxLayout` or `SpPanedLayout`. These two methods are the preferred way to define layouts.

Note the possibility to define a class-side accessor e.g. `defaultLayout` will remain for those who prefer it.

This new design reflects the dynamic nature of layouts in Spec2, and the fact that you can compose them using directly presenter instances, not forcing you to declare upfront sub presenters in instance variable and then use their names as it was done in Spec1.0.
It is, however, possible that there are cases where you want a layout "template"... so you still can do it.


### A running example

To be able to play with the layouts defined in this chapter, 
we define a simple presenter named `SpTwoButtons`.
 
``` 
SpPresenter << #SpTwoButtons 
    slots: { #button1 . #button2 }; 
    package: 'CodeOfSpec20BookThreePillar'
```

We define a simple `initializePresenters` method as follows: 

```
SpTwoButtons >> initializePresenters 
    button1 := self newButton. 
    button2 := self newButton. 

    button1 label: '1'.
    button2 label: '2'.
```



### BoxLayout (SpBoxLayout and SpBoxConstraints)

The class `SpBoxLayout` displays presenters in an ordered sequence of boxes. 
A box can be horizontal or vertical and presenters are ordered top to down or left to right following the direction decided. A box layout can be composed of other layouts.


![Two buttons placed horizontally from left to right.](figures/TwoButtonsLeftToRight.png width=50&label=TwoButtonsLeftToRight) 

Let us defined a first simple layout as follows and whose result is displayed in  Fig. *@TwoButtonsLeftToRight@*.
What we see is that by default a presenter expand its size to fit the space of its container. 
```
SpTwoButtons >> defaultLayout
     ^ SpBoxLayout newLeftToRight
        add: button1; 
        add: button2; 
        yourself
```

An element in a vertical box will use all available horizontal space, and fill
vertical space according to the rules. This is inversed in an horizontal box.

We can refine this layout to indicate that the subpresenters should not expand to their container using the message `add:expand:`. The result is shown in Figure *@TwoButtonsLeftToRightExpanded@*.

```
SpTwoButtons >> defaultLayout
    ^ SpBoxLayout newLeftToRight
		add: #button1 expand: false ; 
		add: #button2 expand: false ;
		yourself
```

![Two buttons placed horizontally from left to right but not expanded.](figures/ThreeButtons.png width=50&label=ThreeButtons) 

The full message to add presenters is: `add:expand:fill:padding:`
- expand - when true, the new child is to be given extra space allocated to box. The extra space is divided evenly between all children that use this option.
- fill - when true, the space given to child by the expand option is actually allocated to child, rather than just padding it. This parameter has no effect if `expand` is set to `false`.
- padding  - extra space in pixels to put between this child and its neighbors, over and above the global amount specified by “spacing” property. If a child is a widget at one of the reference ends of box, then padding pixels are also put between child and the reference edge of box.


To illustrate a bit this API, we add another button to the presenter and change the `defaultLayout` method as follows. The result
is shown in Fig *@TwoButtonsLeftToRightExpanded@*. 
We want to stress however that it is better not to use a fixed width or a padding.

```smalltalk
SpTwoButtons >> defaultLayout 
    ^ SpBoxLayout newTopToBottom
        spacing: 15;
        add: button1 expand: false fill: true padding: 5;
        add: button2 withConstraints: [ :constraints | constraints width: 30; padding: 5];
        addLast: button3 expand: false fill: true padding: 5;
    yourself
```




![Three buttons placed from top to bottom.](figures/TwoButtonsLeftToRightNotExpanded.png width=50&label=TwoButtonsLeftToRightExpanded) 




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

As of this writing, we cannot add a box layout to a grid.

### Paned layout (SpPanedLayout and SpPanedConstraints)

A paned layout is like a Box Layout (it places childen in vertical or horizontal
fashion), but it will add a splitter in between, that user can drag to resize the panel.
In exchange, a paned layout can have just two children. Position Indicates
original position of splitter. It can be nil (then it defaults to 50%) or It can
be a percentage (e.g. 30 percent)

```smalltalk
SpPanedLayout newHorizontal position: 80 percent;
    add: acceptButton;
    add: cancelButton;
    yourself.
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



### Layout and reuse

For example, consider the following artificial example of a two button UI that has two different layouts: horizontal and vertical.
 
 

We define two class method method returning different layouts.
Note that we could define such methods on the instance side too and we define them on the class side to be able to get such layouts without an instance of the class.

``` 
SpTwoButtons class >> buttonRow 
     
    ^ SpBoxLayout newLeftToRight 
        add: #button1; add: #button2; 
        yourself 
``` 
 
``` 
SpTwoButtons class >> buttonCol 
    ^ SpBoxLayout newTopToBottom 
        add: #button1; add: #button2; 
        yourself 
``` 
 
Note that when we define the layout at the class level, we use a symbol whose name is the corresponding instance variable. Hence we use `#button2` to refer to the presenter stored in the instance variable `button2`. This mapping can be customized at the level of the presenter but we do not present this because we never got the need for it. 


We define a `defaultLayout` merthod just invoking one of the previously defined method.
``` 
SpTwoButtons >> defaultLayout 
    ^ self class buttonRow 
```

We define also a `defaultLayout` method so that the presenter can be opened without defining a given layout. 

##### With openWithLayout:
This UI can be opened in multiple ways: 

- `SpTwoButtons new openWithLayout: SpTwoButtons buttonRow` places the buttons in a row. 
- `SpTwoButtons new openWithLayout:  SpTwoButtons buttonCol` places them in a column. 


Now we can do better and define two instance level methods to encapsulate the layout configuration. 

```
SpTwoButtons >> beCol
    self layout: self class buttonCol
```

```
SpTwoButtons >> beRow
    self layout: self class buttonRow
```

Now we can write the following script

```
SpTwoButtons new 
    beCol;
    open 
```


### Specifying a layout when reusing a presenter 

Having multiple layouts for a presenter implies that there is a way to specify the layout to use when a presenter is reused. This is simple we use the method `layout:`.
Here is an example. 
We create a new presenter named: `SpButtonAndListH`.
 
``` 
SpPresenter << #SpButtonAndListH 
    slots: { #buttons . #list }; 
    package: 'CodeOfSpec20BookThreePillar'
```


```
SpButtonAndListH >> initializePresenters 
    buttons := self instantiate: SpTwoButtons. 
    list := self newList. 
    list items: (1 to: 10). 
``` 

``` 
SpButtonAndListH >> initializeWindow: aWindowPresenter 
    aWindowPresenter title: 'SuperWidget' 
``` 


```
SpButtonAndListH >> defaultLayout 
 
    ^ SpBoxLayout newLeftToRight 
          add: buttons;
          add: list; 
          yourself 
```


This `SpButtonAndListH ` class results in a SuperWidget window as shown in Figure *@fig_alternativeButton@*.  
It reuses the `SpTwoButtons` widget, and places all three widgets in a horizontal order because the `SpTwoButtons` widget will use the `buttonRow` layout method. 
 
![Screen shot of the UI with buttons placed horizontally](figures/alternativeButton.png width=50&label=fig_alternativeButton) 
 
Alternatively, we can create `TBAndListV` class as a subclass of `SpButtonAndListH ` and only change the `defaultLayout` method as below.
It specifies that the reused `buttons` widget should use the `buttonCol` layout method, and hence results in the window shown in
Figure
*@fig_SuperWidget@*.


![Screen shot of the UI with buttons placed vertically](figures/SuperWidget.png width=50&label=fig_SuperWidget) 



``` 
SpButtonAndListH << #TButtonAndListV 
    package: 'CodeOfSpec20BookThreePillar'
``` 
 
```
initializePresenters

    super initializePresenters.
    buttons beCol
```


##### Alternative to declare subcomponent layout choice.

The alternative is to define a new method `defaultLayout` and to use the `add:layout:`. 

We define a different presenter

```
SpButtonAndListH << #TButtonAndListV2
    package: 'CodeOfSpec20BookThreePillar'
```

We define a new `defaultLayout` method as follows: 

``` 
SpButtonAndListV2 >> defaultLayout

    ^ SpBoxLayout new
        add: buttons layout: #buttonCol;
        add: list;
        yourself
```

Note the use of the method `add:layout:` with the selector of the method returning the layout configuration 
here #buttonCol. This is normal since we cannot access state of a subcomponent at this moment.



Note that we can change the layout dynamically from an inspector as shown in *@figTweak@*.

![Tweaking and playing interactively with layouts from the inspector.](figures/Interactive.png width=100&label=figTweak) 


