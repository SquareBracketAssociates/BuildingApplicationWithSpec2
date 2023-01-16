## Lists, Tables and Trees

status: definitively missing

An important part of user interfaces is about displaying lists of data. 
Such lists can be structured as tables, plain lists but also trees supporting nesting of data.

Spec proposes three main presenters: `SpListPresenter`, `SpTreePresenter` and `SpTablePresenter`. 
In addition it offers `SpComponentListPresenter` which allows one to embedd any presenter in a list. 
In this chapter we present some of the functionality of such presenters.

### Lists

Creating a list is a simple as instantiating a `SpListPresenter` and specifying a list of items that the list should display.
The following script illustrates this and the result is shown in Figure *@figSimpleList@*.

```
SpListPresenter new
	items: self environment allClasses;
	open
```

![A simple list showing class names](figures/List1Simple.png width=60&label=figSimpleList)

###### Controlling item display.
By default a list item is displayed using the result of the `displayString` message sent to the item.
We can configure a list to apply a block to control the display of each item using the message `display:`.
The following script configures a list presenter to display the name of the methods of the class `Point` instead of showing the result of `printString`. See Figure *@figSimpleList2@*.

```
SpListPresenter 
	items: Point methods;
	display: [ :item | item selector ];
	open
```

![A simple list controlling the way items are displayed](figures/List1Simple2.png width=60&label=figSimpleList2)

### About multiple/single selection

Lists can support multiple selection or not.
The message `beMultipleSelection` controls such aspect.

```
SpListPresenter new
	items: self environment allClasses;
	beMultipleSelection;
	open
```



Since selection can hold multiple item, there is an impact on the protocol to react to selection changes.
Indeed, lists, filtering list, trees, and tables offers the `whenSelectionChangedDo:`API and not `whenSelectedItemDo:`.
The argument of the block is then a selection instance of `SingleSelectionMode` or `MultipleSelectionMode` (`SpSingleSelectionMode`, `SpMultipleSelectionMode`, or `SpTreeMultipleSelectionMode` and `SpTreeSingleSelectionMode`).


Here is a typical use case of the method `whenSelectionChangedDo:`.

```
connectPresenters

	changesTree whenSelectionChangedDo: [ :selection | 
		selection selectedItem
			ifNil: [ textArea text: '' ]
			ifNotNil: [ :item | textArea text: (self buildDiffFor: item) ] ]
```


#### Drag and Drop
Lists and other container structures supports drag and drop.
The following script shows how to configure two lists to support drag from one and dropping in another.

```
| list1 list2 |
list1 := SpListPresenter new
list	
	items: #( 'abc' 'def' 'xyz' );
	dragEnabled: true.

list2 := SpListPresenter new.
list2	dropEnabled: true;
	wantsDrop: [ :transfer | transfer passenger allSatisfy: #isString ];
	acceptDrop: [ :transfer | list2 items: list2 items , transfer passenger ].
```

```
SpPresenter new
	layout: (SpBoxLayout newLeftToRight
		 add: list1;
		 add: list2;
		 yourself);
	open
```


- `dragEnabled:`
- `dropEnabled:`
- `wantsDrop: [ :transfer | transfer passenger allSatisfy: #isString ]`
- `acceptDrop: [ :transfer | list2 items: list2 items , transfer passenger ]`


### Decorating elements





```
SpListPresenter new
	items: self environment allClasses;
	displayIcon: [ :aClass | self iconNamed: aClass systemIconName ];
	displayColor: [ :aClass | 
		(aClass name endsWith: 'Test')
			  ifTrue: [ Color green ]
			  ifFalse: [ Smalltalk ui theme textColor ] ];
	displayItalic: [ :aClass | 
		aClass name includesSubstring: 'abstract' caseSensitive: false ];
	displayBold: [ :aClass | aClass hasSubclasses ];
	displayUnderline: [ :aClass | aClass numberOfMethods > 10 ];
	open
```

### Filtering List



### Component List

### Tables

### Trees

### Conclusion