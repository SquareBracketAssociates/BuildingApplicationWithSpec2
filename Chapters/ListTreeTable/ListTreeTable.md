## Lists, Tables and Trees

status: definitively missing

An important part of user interfaces is about displaying lists of data. 
Such lists can be structured as tables, plain lists but also trees supporting nesting of data.

Spec proposes three main presenters: `SpListPresenter`, `SpTreePresenter` and `SpTablePresenter`. 
In addition it offers `SpComponentListPresenter` which allows one to embedd any presenter in a list. 
In this chapter we present some of the functionality of such presenters.

### Lists



### About multiple selection

```
SpListPresenter new
	items: self environment allClasses;
	beMultipleSelection;
	open;
	yourself
```



#### Drag and Drop

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

### Component List

### Tables

### Trees

### Conclusion