## Customizing your Inspector

status: should do another pass
status: spellchecked


An Inspector is a tool that is used to look and interact with objects.
In Pharo, inspecting an object means opening this tool and interacting with your object. It is a key tool for developing in Pharo. 
It allows one to navigate the object structure, look at the state of the variables, modify their state, or send messages. 
An inspector can show other information and you can also extend it to display the information that is best suited for you.
This is what we will see in this chapter.


### A first look at the inspector

You can inspect the result of an execution by selecting the code and using the shortcut `Cmd/ctrl + i` or `right-click + "Do & inspect it"`. This will execute the code and open an inspector on the result. 

By inspecting `1/3` we get the following inspector:

![Raw tab.](figures/Inspector_raw_tab.png width=60)

![Raw tab areas.](figures/Inspector_raw_tab_areas.png width=60)

The tree-framed areas in the next pictures are :
1. This text starts with the **class** of the inspected object. Here we have an instance of Fraction.
2. This is the raw view object. It shows the internal state of the object. Here the fraction has a `numerator` instance variable holding the value 1, and a `denominator` instance variable holding the value 3. 

This is a tree list. When you see the small grey triangle on the left side, by clicking it you can unfold the state of the object inside the instance variable. 
By clicking on an instance variable you can also open a new inspector pane.

This is recursive: if you click on more variables, more panes will open. By default, only the last two panes are visible at any time. You can use the small rectangles at the bottom of the window to navigate in the opened pane. You can also choose how many panes and showing at the same time and which one by clicking on the small navigation rectangles. 

%Here is a small demonstration :
%![Navigating in the inspector](figures/Inspector_pane_navigation.gif width=40)

3. This last area is the evaluator. In this area, you can write expressions and evaluate them like you would do it in the playground. In a given evaluator, `self` refers to the inspected object. This can be seen in the raw view above the evaluator showing the value of the `self` variable.

### The pane toolbar

Each inspector pane has the following toolbar :

![Toolbar.](figures/Inspector_pane_toolbar.png width=40)

- The **last button** allows one to open a browser on the class of the inspected object. It can be used to check for available methods to use in the evaluator
- The **green glasses** button opens another inspector window on the current object
- The **circling arrows** button allows one to refresh the current view of the object. Fields of an object are not updated live, so if the object is modified from elsewhere, the new values will only show if the refresh button is used.
- The **triangle button** is related to object-centric debugging. It allows one to put breakpoints on state access (read and/or write) of a specific object. The following animation shows how to put a breakpoint on writing an instance variable, the `Breakpoints` tab listing breakpoints on the current object, and how to deactivate one.

%![Breakpoints](figures/Inspector_breakpoints.gif width=60)

### The Meta tab: searching for methods and class hierarchy
The `Meta` tab is the last one that is available on most objects.
On the left it shows the class hierarchy of the current object's class and on the right the methods available. Clicking on parent classes in the hierarchy will show methods implemented in this class on the right. Selecting a method will display its source code at the bottom of the tab.


![Meta tab.](figures/Inspector_meta_tab.png width=60)

### Creating custom tabs

If you used the inspector a bit, you might have noticed that some objects have additional tabs showing up in the inspector.
For example, both `Float`s and `Integer`s have their first tabs showing different representations of the numbers:

![Inspecting numbers.](figures/Inspector_numbers_tabs.png width=60)

Another example is the `FileReference` class. When a file reference is inspected, according to the type of the file, different tabs show up with relevant information.


Creating a new tab is as simple as calling existing Spec presenters or defining new ones for your specific case. For example, you can define a pane displaying a specific Roassal visualization. Roassal 3 visualizations can be embedded in a presenter by sending the `asPresenter` message to an instance of `RSCanvas`.

The following sections will explain how to add a few additional tabs to instances of `OrderedCollection`. This class already has a custom tab showing the list of its items which is defined by its superclass `Collection`.

### Adding a tab with text

Let's add a first tab containing a text describing the first element of the collection. Define the following method :

```Smalltalk
OrderedCollection << inspectionFirstElement

	<inspectorPresentationOrder: 1 title: 'First Element'>
	^ SpTextPresenter new text: 'The first element is ', self first asString; beNotEditable; yourself
```

`<inspectorPresentationOrder: 1 title: 'First Element'>` is a pragma that is detected when creating an inspector on an object. When creating an inspector on an instance of `OrderedCollection`, this method will now be used to generate a tab. The title of the tab will be `First Element`, it will be in position 1 in the order of tabs.

The content of the tab is returned by the method. Here we are creating a text presenter (`SpTextPresenter`) with the content we want and we specify it should not be editable. This gives us the following result:

![First element tab.](figures/Inspector_expension_first_element.png width=60)

We notice that our new tab is in the second position. This is because in `Collection<<inspectionItems:` (the method defining the Items tab) the order parameter is 0.

### Adding a tab with a table and conditions on when to display it

Let's create a new tab that will display a table only when the collection contains only numbers. It will show each number and the results of 2 times that number.

First let's create the tab with the table:

```Smalltalk
OrderedCollection << inspectionMultipliedByTwo

	<inspectorPresentationOrder: 10 title: 'Multiply by 2'>
	| presenter |
	presenter := SpTablePresenter new
		             addColumn:
			             ((SpStringTableColumn
				               title: 'Item'
				               evaluated: #yourself)
				              width: 30;
				              yourself);
		             addColumn: (SpStringTableColumn
				              title: 'Multiply by 2'
				              evaluated: [ :each | each * 2 ]);
		             items: self;
		             beResizable;
		             yourself.
	^ presenter
```
If we inspect a collection of numbers we get the following tab:
![Mutliplied by 2 tab](figures/Inspector_expension_multiplied_by_two.png width=60)

However if the collection contains elements that are not numbers, the tab crashes and looks like a red rectangle. 
By defining a method with the name `<name of the method defining the tab>Context:` we can specify when we want to activate a given tab. For example:

```Smalltalk
OrderedCollection << containsOnlyNumbers 
	^ self allSatisfy: [ :each | each isNumber  ]
```
```Smalltalk
OrderedCollection << inspectionMultipliedByTwoContext: aContext
	^ aContext active: self containsOnlyNumbers.
```

These two methods will ensure that the tab will be displayed only when there are only numbers in the collection.


### Adding a raw view of a specific element of the collection and removing the evaluator

We can also add a tab showing the raw view of the max value:

```pharo
OrderedCollection << inspectionIMaxValue
	<inspectorPresentationOrder: 5 title: 'Max Value'>
	^ StRawInspectionPresenter on: self max.
```

```pharo
OrderedCollection << inspectionIMaxValueContext: aContext
	^ aContext active: self containsOnlyIntegers.
```

![Inspect max value tab.](figures/Inspector_expension_max.png width=60)

However as we can see above, the `self` in the evaluator does not match the `self` in the max value which is confusing so we will hide the evaluator.

```Smalltalk
OrderedCollection << inspectionIMaxValueContext: aContext
	aContext withoutEvaluator.
	^ aContext active: self containsOnlyIntegers.
```

By reinspecting the same collection we now get:
![Removing the evaluator.](figures/Inspector_expension_max_without_evaluator.png)
s

### Adding Roassal charts

As said above, Roassal allows one to build visualizations.
The library includes some common graphs like the histogram for example.
Let's add a histogram of the values if there are only numbers in the collection :

```Smalltalk
OrderedCollection << inspectionIntegerHistogram
	<inspectorPresentationOrder: -1 title: 'Histogram'>
	
	| plot |
	plot := RSHistogramPlot new x: self.
	^ plot asPresenter
```

```
OrderedCollection << inspectionIntegerHistogramContext: aContext 
	
	aContext active: self containsOnlyIntegers.
	aContext withoutEvaluator.
```

By inspecting `{ 1 . 1 . 3 . 2 . 5 . 2. 2 . 1. 9. 3 . 2. 2. 5 . 7 . 7 . 8  } asOrderedCollection` we get Figure *@hist@*.

![Histogram tab](figures/Inspector_expension_histogram.png label=hist&width=60)


### Conclusion 

In this chapter, we presented briefly the inspector and how you can specialize its panes to shape the way you can see and interact with your objects.
We presented how to define conditional panes as well as embed visualizations.
