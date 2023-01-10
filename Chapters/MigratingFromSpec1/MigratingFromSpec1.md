## Migrating from Spec1 
 
@cha_migrating 
 
status: need more love or to be removed. 
 
Here is a little list of points that you should consider when you migrate your applications from Spec1 to Spec2. 
 
 
### Picking the right superclass 
 
Your presenter or interaction model should now inherit from `SpPresenter` or `SpPresenterWithModel` and not from `ComposablePresenter`.  
 
Note that `SpPresenterWithModel` avoids the definition of the method `setModelBeforeInitialization:`. 
 
 
 
 
### Definition hook renaming 
 
Some of the hook methods got renamed to convey better what they are doing: 
 
- `initializeWidgets` is renamed to `initializePresenters` 
- `initializePresenter` is renamed to `connectPresenters` 
 
 
There is not need to define a defaultSpec class side method. You can now simply define an `initializeLayout` method and call it. 
 
!!todo help here 
 
### Layouts 
 
 
 
`SpecLayout composed newColumn:` and `newRow:` are now replaced by `SpBoxLayout newTopToBottom` and `newLeftToRight` messages. 
Here you see the conversion 
 
``` 
SpecLayout composed
	newColumn: [ : column | 
		column
			newRow: [ :nameRow |
				nameRow 
					add: #nameLabel width: self labelSize;
					add: #nameInputField ]
			height: self inputTextHeight;
			newRow: [ :versionRow |
				versionRow 
					add: #versionLabel width: self labelSize;
					add: #versionInputField ]
			height: self inputTextHeight] 
``` 
 
 
``` 
SpBoxLayout newTopToBottom 
	add: (SpBoxLayout newLeftToRight
			add: #nameLabel expand: false;
			add: #nameInputField expand: false;
			yourself);
	add: (SpBoxLayout newLeftToRight
			add: #versionLabel expand: false;
			add: #versionInputField expand: false;
			yourself);
	yourself 
``` 
 
 
Use #add:expand: to add items to a layout 
 
### Others  
 
 
- You do not need setters and getters to hold subpresenters anymore, so you can remove them. 
 
 
- You can remove static labels by just declaring them in the layout \(e.g. `layout add: 'Name:' expand: false`\) 
 
 
- Panels are replaced by dynamic layouts 
 
!!todo help I could not understabd the last sentence