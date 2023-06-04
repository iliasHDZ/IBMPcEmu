#include "monochromeDisplay.hpp"

#include <SDL2/SDL.h>
#include <stdio.h>

MonochromeDisplay::MonochromeDisplay() {

}

static void displayRun(MonochromeDisplay* display) {
    display->run();
}

void MonochromeDisplay::run() {
    SDL_UpdateWindowSurface(window);
    running = true;

    SDL_Event e;
    while (running) {
        while (SDL_PollEvent(&e)) {
            if (e.type == SDL_QUIT)
                running = false;
        }
    }
}

void MonochromeDisplay::close() {
    running = false;
    thread.join();
}

bool MonochromeDisplay::init() {
    screen_width  = MONO_SCREEN_WIDTH;
    screen_height = MONO_SCREEN_HEIGHT;
    char_width  = MONO_CHAR_WIDTH;
    char_height = MONO_CHAR_HEIGHT;
    scale = 2;

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        printf("Couldn't init SDL. SDL_Error: %s\n", SDL_GetError());
        return false;
    }

    window = SDL_CreateWindow(
        "IBM Monochrome Display | 720x350 res | 80x25 chars | 9x14 char box",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        screen_width  * char_width  * scale,
        screen_height * char_height * scale,
        SDL_WINDOW_SHOWN
    );

    if (window == NULL) {
        printf("Couldn't create SDL_Window. SDL_Error: %s\n", SDL_GetError());
        return false;
    }

    surface = SDL_GetWindowSurface(window);

    thread = std::thread(displayRun, this);
}