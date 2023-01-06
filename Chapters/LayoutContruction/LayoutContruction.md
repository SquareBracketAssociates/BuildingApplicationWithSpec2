## TBRewritten Layout Construction
	instanceVariableNames: 'list button button2 text'
	classVariableNames: ''
	package: 'Spec-BuildUIWithSpec'
	button := self newButton.
	button2 := self newButton.
	list := self newList.
	text := self newText.
	button label: 'i am a button'.
	button label: 'me too!'.
	^ SpecLayout composed
		newRow: [ :row | row add: #list; add: #button ];
		yourself
 le := LayoutExample new.
 (le openWithSpec: #oneRow) title: 'RowOfWidgets'.
 le title: 'RowOfWidgets'
	^ SpecLayout composed
		newColumn: [ :col | col add: #list; add: #button ];
		yourself
 le := LayoutExample new.
 (le openWithSpec: #oneColumn) title: 'ColumnOfWidgets'.
	^ SpecRowLayout composed
			add: #list; add: #button;
			yourself.
	^ SpecColumnLayout composed
			add: #list; add: #button;
			yourself
	^ SpecColumnLayout composed
			newRow: [ :row | row add: #text ];
			newRow: [ :row | row add: #button; add: #button2 ];
			yourself
	^ SpecRowLayout composed
		newColumn: [ :col | col add: #list];
		newColumn: [ :col |
			col
				add: #text;
				newRow: [ :row |
					row
						add: #button;
						add: #button2]
		];
		yourself
	^ SpecColumnLayout composed
		newRow: [ :row | row add: #list];
		newRow: [ :row |
			row
				add: #text;
				newRow: [ :inRow |
					inRow
						add: #button;
						add: #button2]
		];
		yourself
	^ SpecColumnLayout composed
			add: #list;
			addSplitter;
			add: #button;
			yourself
	^ SpecLayout composed
		newRow: [ :row | row add: #list; add: #button] height: 30;
		yourself
	^ SpecLayout composed
		newColumn: [ :col | col add: #list; add: #button] width: 50;
		yourself
	^ SpecColumnLayout composed
		newRow: [ :row | row add: #list] top: 0 bottom: 0.2 ;
		newRow: [ :row |
			row
				newColumn: [:c | c add: #text] left: 0 right: 0.45;
				newColumn: [ :c |
					c newRow: [ :inRow |
						inRow
							add: #button;
							add: #button2]] left: 0.55 right: 0
		] top: 0.8 bottom: 0 ;
		yourself
	^ SpecLayout composed
		add: #button top: 10 bottom: 200 left: 10 right: 10;
		yourself
	^ SpecLayout composed
		add: #button origin: (0.25 @ 0.25) corner: (0.75 @ 0.75);
		yourself
	^ SpecLayout composed
		add: #button top: 0.25 bottom: 0.25 left: 0.25 right: 0.25;
		yourself
	^ SpecLayout composed
		add: #button origin: (0 @ 0) corner: (1 @ 0.5)
				 offsetOrigin: (10 @ 10) offsetCorner: (-10 @ -5);
		add: #button2 origin: (0 @ 0.5 ) corner: (1 @ 1)
				 offsetOrigin: (10 @ 5) offsetCorner: (-10 @ -10);
		yourself
	^ SpecLayout composed
		newRow: [ :row |
				row add: #buttonHappy; add: #buttonNeutral; add: #buttonBad ]
			origin: 0 @ 0 corner: 1 @ 0.7;
		newRow: [ :row |
				row add: #screen ]
			origin: 0 @ 0.7 corner: 1 @ 1;
		yourself