## Customizing your Inspector

status: should do another pass
status: spellchecked


An Inspector is a tool that is used to look at and interact with objects. In Pharo, inspecting an object means opening this tool and interacting with your object. It is a key tool when developing in Pharo. It allows one to navigate the object structure, look at the state of the variables, change their value, or send messages. An inspector can show other information and you can extend it to display the information that is best suited for you. This is what we will see in this chapter.


### A first look at the inspector

You can inspect the result of an execution by selecting the code and using the shortcut `Cmd/ctrl + i` or `right-click + "Do & inspect it"`. This will execute the code and open an inspector on the result.

By inspecting `1/3` we get the inspector shown in Figure *@InspectorWithRawTab@*.

![An inspector with the Raw tab selected. % width=60&anchor=InspectorWithRawTab](figures/InspectorRawTab.png)

![Inspector areas. % width=60&anchor=InspectorWithThreeAreas](figures/InspectorRawTabAreas.png)

There are three areas in an inspector. They are highlighted in Figure *@InspectorWithThreeAreas@*.

1. This text starts with the **class** of the inspected object. Here we have an instance of Fraction.
2. This is the raw view on the object. It shows the internal state of the object. Here the fraction has a `numerator` instance variable holding the value 1, and a `denominator` instance variable holding the value 3.
3. The last area is the evaluator. In this area, you can write expressions and evaluate them like you would in the playground. In an evaluator, `self` refers to the inspected object. This object can be seen in the raw view above the evaluator showing the value of the `self` variable.

The raw view on the object is a tree list. By clicking the small grey triangle on the left side, you can unfold the state of the object held by the instance variable. By clicking on an instance variable, you open a new inspector pane.

This is recursive: if you click on more variables, more panes will open. By default, only the last two panes are visible at any time. You can use the small rectangles at the bottom of the window to navigate between panes.

%Figure *@InspectorNavigation@* is a small demonstration.

KDH: adding a GIF file does not work. We need an alternative or remove this part

%![Navigating in the inspector](figures/InspectorPaneNavigation.gif %anchor=InspectorNavigation&width=40)

### The inspector toolbar

Each inspector has the toolbar shown in Figure *@InspectorToolbar@*:

![Toolbar.](figures/InspectorToolbar.png width=40&label=InspectorToolbar)

- The **triangle button** is related to object-centric debugging. It allows putting breakpoints on state access (read and/or write) of a specific object.
- The **circling arrows** button allows refreshing the current view of the object. Fields of an object are not updated live, so if the object is modified from elsewhere, the new values will only show after this button is used.
- The **green glasses** button opens another inspector window on the current object.
- The **last button** allows opening a browser on the class of the inspected object. It can be used to check for available methods to use in the evaluator.


### The Breakpoints tab: managing breakpoints

KDH: this section was/is missing.

The following animation shows how to put a breakpoint on writing an instance variable, the breakpoints listing on the current object, and how to deactivate one.

KDH: adding a GIF file does not work. We need an alternative or remove this part

%![Breakpoints](figures/InspectorBreakpoints.gif width=60&label=InspectorBreakpoints)

### The Meta tab: class hierarchy and searching methods

The `Meta` tab is the last one that is available for most objects. See Figure *@InspectorMetaTab@*. On the left, it shows the hierarchy of the current object's class. On the right, it shows the available methods. Clicking on parent classes in the hierarchy will show methods implemented in this class on the right. Selecting a method will display its source code at the bottom of the tab.

![Meta tab. % width=60&anchor=InspectorMetaTab](figures/InspectorMetaTab.png)

### Creating custom tabs

If you used the inspector a bit, you may have noticed that some objects have additional tabs showing up in the inspector.
For example, both `Float`s and `Integer`s have their first tabs showing different representations of numbers, as shown in Figure *@InspectorForNumbers@*.

![Inspecting numbers. % width=60&anchor=InspectorForNumbers](figures/InspectorNumbersTabs.png)

Another example is the `FileReference` class. When a file reference is inspected, according to the type of the file, different tabs show up with relevant information.


Creating a new tab is as simple as reusing existing Spec presenters or defining new ones for your specific case. For example, you can define a tab displaying a specific Roassal visualization.

The following sections will explain how to add a few additional tabs to instances of `OrderedCollection`. This class already has a custom tab showing the list of its items which is defined by its superclass `Collection`.

### Adding a tab with text

Let's add a first tab containing a text describing the first element of the collection. Define the following method:

```
OrderedCollection << inspectionFirstElement

  <inspectorPresentationOrder: 1 title: 'First Element'>

  ^ SpTextPresenter new
    text: 'The first element is ', self first asString;
    beNotEditable;
    yourself
```

`<inspectorPresentationOrder: 1 title: 'First Element'>` is a pragma that is detected when creating an inspector on an object. When creating an inspector on an instance of `OrderedCollection`, this method will now be used to generate a tab. The title of the tab will be `First Element`, it will have position 1 in the order of tabs.

The content of the tab is returned by the method. Here we are creating a text presenter (`SpTextPresenter`) with the content we want and we specify that it should not be editable. This gives us the result shown in Figure *@InspectorFirstElementTab@*.

![First element tab.](figures/InspectorExpansionFirstElement.png width=60&label=InspectorFirstElementTab)

Notice that our new tab is in the second position. This is because in `Collection<<inspectionItems:` (the method defining the Items tab) the order parameter is 0.

### Adding a tab with a table and conditions on when to display it

Let's create a new tab that will display a table if the collection contains only numbers. It will show each number and the result of multiplying that number with 2.

First let's create the tab with the table:

```
OrderedCollection << inspectionMultipliedByTwo

  <inspectorPresentationOrder: 10 title: 'Multiply by 2'>

  | itemColumn multipliedByTwoColumn |
  itemColumn := SpStringTableColumn
    title: 'Item'
    evaluated: #yourself.
  itemColumn width: 30.
  multipliedByTwoColumn := SpStringTableColumn
    title: 'Multiply by 2'
    evaluated: [ :each | each * 2 ].
  ^ SpTablePresenter new
      addColumn: itemColumn;
      addColumn: multipliedByTwoColumn;
      items: self;
      beResizable;
      yourself
```

When we inspect a collection of numbers we see the tabs shown in Figure *@InspectorMultipliedByTwoTab@*.

![Multiplied by 2 tab. % width=60&anchor=InspectorMultipliedByTwoTab](figures/InspectorExpansionMultipliedByTwo.png)

However if the collection contains elements that are not numbers, the tab crashes and looks like a red rectangle. By defining a method with the name `<name of the method defining the tab>Context:` we can specify when we want to activate a given tab. For example:

```
OrderedCollection << inspectionMultipliedByTwoContext: aContext

  ^ aContext active: self containsOnlyNumbers
```

```
OrderedCollection << containsOnlyNumbers

  ^ self allSatisfy: [ :each | each isNumber ]
```

These two methods will ensure that the tab will be displayed only when there are only numbers in the collection.


### Adding a raw view of a specific element of the collection and removing the evaluator

We can also add a tab showing the raw view of the max value:

```
OrderedCollection << inspectionMaxValue

  <inspectorPresentationOrder: 5 title: 'Max Value'>

  ^ StRawInspectionPresenter on: self max
```

```
OrderedCollection << inspectionMaxValueContext: aContext

  ^ aContext active: self containsOnlyIntegers
```

![Inspect max value tab. %width=60&anchor=InspectorMaxValueTab](figures/InspectorExpansionMax.png)

However as we can see in Figure *@InspectorMaxValueTab@*, the `self` in the evaluator does not match the `self` in the max value, which is confusing. So we will hide the evaluator.

```
OrderedCollection << inspectionMaxValueContext: aContext

  aContext withoutEvaluator.
  ^ aContext active: self containsOnlyIntegers
```

By reinspecting the same collection we see the inspector in Figure *@InspectorWithoutEvaluator@*.

![Removing the evaluator. % width=60&anchor=InspectorWithoutEvaluator](figures/InspectorExpansionMaxWithoutWvaluator.png)

### Adding Roassal charts

As said above, Roassal allows one to build visualizations. The library includes some common graphs like a histogram. Let's add a histogram of the values if there are only numbers in the collection. Roassal 3 visualizations can be embedded in a presenter by sending the `asPresenter` message to an instance of `RSBuilder`. In the code below, `RSHistogramPlot` is a subclass of `RSBuilder`.

```
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

By inspecting `{ 1 . 1 . 3 . 2 . 5 . 2. 2 . 1. 9. 3 . 2. 2. 5 . 7 . 7 . 8  } asOrderedCollection` we see the inspector shown in Figure *@histogram@*.

![Histogram tab.  %width=60&anchor=histogram](figures/InspectorExpansionHistogram.png)


### Conclusion

In this chapter, we presented briefly the inspector and how you can specialize its tabs and evaluator to shape the way you can see and interact with your objects. We presented how to define conditional tabs, as well as embed visualizations.
