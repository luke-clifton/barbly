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

void sendEvent(void)
{
    NSEvent *e = [NSEvent otherEventWithType: NSEventTypeApplicationDefined location: NSZeroPoint modifierFlags: 0 timestamp: 0 windowNumber: 0 context: nil subtype: 12 data1: 0 data2: 0];
    [NSApp postEvent: e atStart: NO];
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

NSString *newNSString(char *volatile str)
{
    return [[NSString alloc] initWithUTF8String: str];
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

NSMenu *newMenu(char *title)
{
    NSString *t = newNSString(title);
    NSMenu *r = [[NSMenu alloc] initWithTitle: t];
    [t release];
    return r;
}

NSMenuItem *newMenuItem(char *title)
{
    NSString *t = newNSString(title);
    NSMenuItem *r = [[NSMenuItem alloc] initWithTitle: t action: NULL keyEquivalent: @""];
    [t release];
    return r;
}

void assignAction(NSMenuItem *mi, void (*ptr)(void))
{
    IOAction *action = [IOAction alloc];
    action->ioaction = ptr;
    mi.target = action;
    mi.action = @selector(callIOAction);
    mi.representedObject = action;
    [action release];
}

void assignSubMenu(NSMenuItem *mi, NSMenu *m)
{
    mi.submenu = m;
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
