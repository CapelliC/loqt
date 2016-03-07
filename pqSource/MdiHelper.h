/*
    pqSource     : interfacing SWI-Prolog source files and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef MDIHELPER_H
#define MDIHELPER_H

#include "pqSource_global.h"

#include <QMenu>
#include <QLabel>
#include <QPointer>
#include <QMdiArea>
#include <QComboBox>
#include <QMainWindow>
#include <QTextCursor>
#include <QMdiSubWindow>

#include "MruHelper.h"
#include "RowColIndicators.h"

/** this class defines the IDE global layout
 *  create widgets for commanders and feedback
 */
class PQSOURCESHARED_EXPORT MdiHelper : public QMainWindow, protected MruHelper
{
    Q_OBJECT

protected:

    MdiHelper(QWidget *parent = 0, Qt::WindowFlags flags = 0) : QMainWindow(parent, flags) {}
    void setupMdi();

    // from MDI example
    void createActions();
    void createMenus();
    void createToolBars();
    void createStatusBar();

    QMdiSubWindow *findMdiChild(const QString &fileName);

    QPointer<QMenu>
        fileMenu,
        editMenu,
        prefMenu,
        debugMenu,
        windowMenu,
        helpMenu;

    QPointer<QToolBar>
        fileToolBar,
        editToolBar,
        debugToolBar,
        helpToolBar;

    QPointer<QComboBox>
        queriesBox;

    QPointer<QAction>
        newAct,
        openAct,
        saveAct,
        saveAsAct,
        switchLayoutDirectionAct,
        switchViewModeAct,
        exitAct,

        cutAct,
        copyAct,
        pasteAct,

        renderViewAct,
        renderClauseAct,
        renderPredicateAct,

        findAct,
        findNextAct,
        findPreviousAct,
        replaceAct,

        viewSWIPrologPrefAct,
        selectColorsAct,
        selectFontAct,
        incFontAct,
        decFontAct,

        makeAct,
        consultAct,
        runAct,
        stopAct,
        stepInAct,
        stepOutAct,
        stepOverAct,
        toggleBPAct,
        watchBPAct,

        closeAct,
        closeAllAct,
        tileAct,
        cascadeAct,
        nextAct,
        previousAct,
        separatorAct,

        helpStartAct,
        helpDocAct,
        aboutAct,
        aboutQtAct,

        viewCallGraphAct,
        viewGraphAct,
        viewGraphIncl,
        commentClauseAct,

        foldClauseAct,
        unfoldClauseAct,
        foldAllAct,
        unfoldAllAct;

    inline QMdiArea* mdiArea() const {
        return qobject_cast<QMdiArea*>(centralWidget());
    }

    inline QWidget* activeMdiChild() const {
        const QMdiSubWindow *w = mdiArea()->activeSubWindow();
        return w ? w->widget() : 0;
    }

    template <class T>
    inline T* activeChild() const {
        return qobject_cast<T*>(activeMdiChild());
    }

    template<class T>
    QList<T*> typedSubWindows() const
    {
        QList<T*> l;
        foreach (auto w, mdiArea()->subWindowList())
            if (auto s = qobject_cast<T*>(w->widget()))
                l << s;
        return l;
    }

    template<class T>
    QList<T*> matchingSubWindows(std::function<bool(T*)> match) const
    {
        QList<T*> l;
        foreach (auto s, typedSubWindows<T>())
            if (match(s))
                l << s;
        return l;
    }

    // associate a string key with a widget
    virtual QString symbol(QWidget *w) {
        // rough approx.
        return w->windowTitle();
    }

    RowColIndicators cursorIndicator;

private slots:
    void updateMenus();
    void updateWindowMenu();
    void setActiveSubWindow(QWidget *window);
    void switchLayoutDirection();
    void switchViewMode();
    void cut();
    void copy();
    void paste();

protected slots:
    void subWindowActivated(QMdiSubWindow *);
    void cursorPositionChanged();
};

#endif // MDIHELPER_H
