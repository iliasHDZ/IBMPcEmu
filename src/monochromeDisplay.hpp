#pragma once

#include "util.hpp"

#include <thread>

#define MONO_SCREEN_WIDTH  80
#define MONO_SCREEN_HEIGHT 25
#define MONO_CHAR_WIDTH  9
#define MONO_CHAR_HEIGHT 14

struct SDL_Window;
typedef struct SDL_Window SDL_Window;

struct SDL_Surface;
typedef struct SDL_Surface SDL_Surface;

class MonochromeDisplay {
public:
    MonochromeDisplay();

    void run();
    
    bool init();

    void close();

private:
    SDL_Window*  window;
    SDL_Surface* surface;

    bool running = false;
    std::thread thread;

    u16 screen_width;
    u16 screen_height;

    u16 char_width;
    u16 char_height;

    u8 scale;
};