#import "AppKit/AppKit.h"
#import "AppKit/NSStatusBar.h"

#include <unistd.h>

void release(id o)
{
    [o release];
}

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

/******************************************************/

@interface MyMenuItem : NSMenuItem
{
}

- (void) dealloc;
@end

@implementation MyMenuItem

- (void) dealloc
{
    if (self.target) [self.target release];
    [super dealloc];
}

@end

/******************************************************/

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

void setTitle(NSStatusItem *si, char *title)
{
    NSString *t = newNSString(title);
	si.button.title = t;
    [t release];
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

MyMenuItem *newMenuItem(char *title, void (*ptr)(void))
{
    NSString *t = newNSString(title);
	MyMenuItem *mi = [[MyMenuItem alloc] initWithTitle: t action: NULL keyEquivalent: @""];
    [t release];
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
