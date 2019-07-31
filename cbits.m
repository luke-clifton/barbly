#import "AppKit/AppKit.h"
#import "AppKit/NSStatusBar.h"

#include <unistd.h>

@interface IOAction : NSObject
{
	@public void (*ioaction)(void);
}

- (void) callIOAction;
@end

@implementation IOAction

- (void) callIOAction
{
	self->ioaction();
}

@end

void initApp(void)
{
	[NSApplication sharedApplication];
}

NSStatusItem *newStatusItem(void)
{
	return [[NSStatusBar systemStatusBar] statusItemWithLength:NSVariableStatusItemLength];
}

NSString *newNSString(char *str)
{
	return [[NSString string] initWithUTF8String: str];
}

// NSAttributedString *newPlainString(char *str)
// {
// 	// ;
// }
// 
// 
// 
// NSAttributedString *newColourString(char *str, 

void setTitle(NSStatusItem *si, char *title)
{
	si.button.title = newNSString(title);
}

void runApp(double period, void (*ptr)(void))
{
	[NSEvent addLocalMonitorForEventsMatchingMask: NSEventMaskPeriodic handler: ^NSEvent * _Nullable (NSEvent *e){
		ptr();
		return nil;
	}];
	[NSEvent startPeriodicEventsAfterDelay: 0 withPeriod: period];
	[NSApp run];
}

NSMenu *newMenu(char *title)
{
	return [[NSMenu alloc] initWithTitle: newNSString(title)];
}

NSMenuItem *newMenuItem(char *title, void (*ptr)(void))
{
	NSMenuItem *mi = [[NSMenuItem alloc] initWithTitle: newNSString(title) action: NULL keyEquivalent: @""];
	if (ptr)
	{
		IOAction *action = [IOAction alloc];
		action->ioaction = ptr;
		mi.target = action;
		mi.action = @selector(callIOAction);
	}
	return mi;
}

void addMenuItem(NSMenu *m, NSMenuItem *i)
{
	[m addItem: i];
}

void setStatusItemMenu(NSStatusItem *si, NSMenu *m)
{
	si.menu = m;
}
