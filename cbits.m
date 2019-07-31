#import "AppKit/AppKit.h"
#import "AppKit/NSStatusBar.h"

#include <unistd.h>

void freeHaskellFunPtr(void (*ptr)(void));

@interface IOAction : NSObject
{
	@public void (*ioaction)(void);
}

- (void) callIOAction;
- (void) dealloc;
@end

@implementation IOAction

- (void) callIOAction
{
	self->ioaction();
}

- (void) dealloc
{
    freeHaskellFunPtr(self->ioaction);
    [super dealloc];
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
    NSString *old = si.button.title;
	si.button.title = newNSString(title);
    if (old)
    {
        [old release];
    }
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
    NSString *t = newNSString(title);
	NSMenu *m = [[NSMenu alloc] initWithTitle: t];
    [t release];
    return m;
}

NSMenuItem *newMenuItem(char *title, void (*ptr)(void))
{
    NSString *t = newNSString(title);
	NSMenuItem *mi = [[NSMenuItem alloc] initWithTitle: t action: NULL keyEquivalent: @""];
    [t release];
	if (ptr)
	{
		IOAction *action = [IOAction alloc];
		action->ioaction = ptr;
		mi.target = action;
		mi.action = @selector(callIOAction);
        // [action release];
	}
	return mi;
}

void addMenuItem(NSMenu *m, NSMenuItem *i)
{
	[m addItem: i];
    [i release];
}

void setStatusItemMenu(NSStatusItem *si, NSMenu *m)
{
    NSMenu *old = si.menu;
	si.menu = m;
    if (old)
    {
        for (NSMenuItem *object in old.itemArray)
        {
            [object.target release];
        }
        [old removeAllItems];
        [old release];
    }
}
