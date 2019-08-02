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

void sendEvent(void)
{
    NSEvent *e = [NSEvent otherEventWithType: NSApplicationDefined location: NSZeroPoint modifierFlags: 0 timestamp: 0 windowNumber: 0 context: nil subtype: 12 data1: 0 data2: 0];
    [NSApp sendEvent: e];
    [e release];
}

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

void runApp(void (*ptr)(void))
{
    [NSEvent addLocalMonitorForEventsMatchingMask: NSEventMaskApplicationDefined handler: ^NSEvent * _Nullable (NSEvent *e){
        ptr();
        return nil;
    }];
    [NSApp run];
}

void sendTerminate(void)
{
    [NSApp terminate: nil];
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

NSMenuItem *newSeparator(void)
{
    return [NSMenuItem separatorItem];
}

void addMenuItem(NSMenu *m, NSMenuItem *i)
{
    [m addItem: i];
}

void setStatusItemMenu(NSStatusItem *si, NSMenu *m)
{
    si.menu = m;
}

