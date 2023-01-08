# Spec2 - style

Spec2 allows you to style your widget, either morphic or Gtk. Here, we'll focus on morphic, which is the only platform  I work on currently.

SpMorphStyle class >> fromStylesheet: aStyle adapter: anAdapter
 "I collect all styles that apply to an adapter.
  I traverse all styles in a very precise order: from more generic to more specific, this way
  the order of the collected elements will be in reverse order of execution."

find style name for morphic element:

SpAbstractMorphicAdapter >> styleName
^ ((self className withoutPrefix: 'SpMorphic') allButLast: 7) uncapitalized

=> SpMorphicLabelAdapter -> Label

We first collect the style for the presenter, then we collect style for is
specific sub-element. 'application' is the default root level, there is no
'application' adapter.

A defined stylesheet has to have always a root element, and this root element
needs to be called '.application'.

So each style follow a cascading style, starting from .application
like
.application.label.header
.application.link
.application.checkBox

A style can be declared through the STON notation.
Each style can follow in on of these category with properties taken from
object messages.

- Container -> SpStyleContainer
- Draw -> SpStyleDraw
- Font -> SpStyleFont
- Geometry -> SpStyleGeometry.

Example:
    Geometry { #hResizing: true }
    Draw { #color:  Color{ #red: 1, #green: 0, #blue: 0, #alpha: 1}}
    Draw { #color: #blue}
    Font { #name: "Lucida Grande", #size: 10, #bold: true }
    Container { #borderColor: Color { #rgb: 0, #alpha: 0 }, #borderWidth: 2, #padding: 5 },

you can define your style globaly, and to your specific presenter, with the 'addStyle:'
message: addStyle: 'section'. This message is specific to the SpAbstractMorphicAdapter backend.

styleSheet
 ^ SpStyleSTONReader fromString: '
.application [
 Font { #name: "Source Sans Pro", #size: 10 },
 Geometry { #height: 25 },
 .label [
  Geometry { #hResizing: true },
  .headerError [Draw { #color:  Color{ #red: 1, #green: 0, #blue: 0, #alpha: 1}}  ],
  .headerSuccess [Draw { #color: Color{ #red: 0, #green: 1, #blue: 0, #alpha: 1}}  ],
  .header [
   Draw { #color: Color{ #rgb: 622413393 }},
   Font { #name: "Lucida Grande", #size: 10, #bold: true } ],
  .shortcut [
   Draw { #color: Color{ #rgb: 622413393 } },
   Font { #name: "Lucida Grande", #size: 10 }
  ],
  .fixed [
   Geometry { #hResizing: false, #width: 100 }
  ],
  .dim [
   Draw { #color : Color{ #rgb: 708480675 } }
  ]
 ],
 .link [  
  Geometry { #hResizing: true }  
 ],
 .button [  
  Geometry { #width: 100 },
  .small [
     Geometry { #width: 26 }
  ]
 ],
 .checkBox [  
  Geometry { #hResizing: true }
 ],
 .radioButton [
  Geometry { #hResizing: true }
 ],
 .dropList [
  Geometry { #width: 150, #hResizing: true }
 ],
 .list [
  Geometry { #width: 150, #hResizing: true, #vResizing: true }
 ],
 .slider [
  Geometry { #width: 150, #hResizing: true }
 ],
 .actionBar [  
  Container {
   #borderColor: Color { #rgb: 0, #alpha: 0 },
   #borderWidth: 2,
   #padding: 5 },
  Geometry { #width: 150, #height: 29, #hResizing: true, #vResizing: false }
 ],
 .menuBar [
  Geometry { #width: 150, #hResizing: true }
 ],
 .actionButton [  
  Geometry { #width: 60, #hResizing: false },
  .showIcon [ Geometry { #width: 25 } ]
 ],
 .toolBar [
  Geometry { #hResizing: true },
  .icons [
   Geometry { #height: 30 }
  ],
  .iconsAndLabel [  
   Geometry { #height: 45 }
  ]
 ],
 .text [
  Geometry { #height: 0 }
 ],
 .code [
  Font { #name : "Source Code Pro", #size : 10 }
 ],
 .codePopover [
  Draw { #color : #transparent },
  .button [
   Geometry { #width : 25 }
  ]
 ],
 .scrollbarPopoverLarge [
  Geometry { #height: 350 }
 ]
]
'
  
styleSheet
 ^ SpStyle defaultStyleSheet, (SpStyleSTONReader
  fromString:
   '
.application [
 Draw { #backgroundColor: #lightRed},
 .section [
   Draw { #color: #green, #backgroundColor: #lightYellow},
   Font {  #name: "Verdana", #size: 12, #italic: true, #bold: true}],
 .disabled [ Draw { #backgroundColor: #lightGreen} ],
 .textInputField [ Draw { #backgroundColor: #blue} ],
 .label [
  Font {  #name: "Verdana", #size: 10, #italic: false, #bold: true},
  Draw { #color: #red, #backgroundColor: #lightBlue}
 ]
]
')
