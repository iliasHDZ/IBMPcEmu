# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.22

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/iliashdz/IBMPcEmu

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/iliashdz/IBMPcEmu/bin

# Include any dependencies generated for this target.
include CMakeFiles/IBMPcEmu.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/IBMPcEmu.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/IBMPcEmu.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/IBMPcEmu.dir/flags.make

CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.o: CMakeFiles/IBMPcEmu.dir/flags.make
CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.o: ../src/cpu8086.cpp
CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.o: CMakeFiles/IBMPcEmu.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/iliashdz/IBMPcEmu/bin/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.o"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.o -MF CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.o.d -o CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.o -c /home/iliashdz/IBMPcEmu/src/cpu8086.cpp

CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/iliashdz/IBMPcEmu/src/cpu8086.cpp > CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.i

CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/iliashdz/IBMPcEmu/src/cpu8086.cpp -o CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.s

CMakeFiles/IBMPcEmu.dir/src/main.cpp.o: CMakeFiles/IBMPcEmu.dir/flags.make
CMakeFiles/IBMPcEmu.dir/src/main.cpp.o: ../src/main.cpp
CMakeFiles/IBMPcEmu.dir/src/main.cpp.o: CMakeFiles/IBMPcEmu.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/iliashdz/IBMPcEmu/bin/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object CMakeFiles/IBMPcEmu.dir/src/main.cpp.o"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT CMakeFiles/IBMPcEmu.dir/src/main.cpp.o -MF CMakeFiles/IBMPcEmu.dir/src/main.cpp.o.d -o CMakeFiles/IBMPcEmu.dir/src/main.cpp.o -c /home/iliashdz/IBMPcEmu/src/main.cpp

CMakeFiles/IBMPcEmu.dir/src/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/IBMPcEmu.dir/src/main.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/iliashdz/IBMPcEmu/src/main.cpp > CMakeFiles/IBMPcEmu.dir/src/main.cpp.i

CMakeFiles/IBMPcEmu.dir/src/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/IBMPcEmu.dir/src/main.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/iliashdz/IBMPcEmu/src/main.cpp -o CMakeFiles/IBMPcEmu.dir/src/main.cpp.s

CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.o: CMakeFiles/IBMPcEmu.dir/flags.make
CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.o: ../src/monochromeDisplay.cpp
CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.o: CMakeFiles/IBMPcEmu.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/iliashdz/IBMPcEmu/bin/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.o"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.o -MF CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.o.d -o CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.o -c /home/iliashdz/IBMPcEmu/src/monochromeDisplay.cpp

CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/iliashdz/IBMPcEmu/src/monochromeDisplay.cpp > CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.i

CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/iliashdz/IBMPcEmu/src/monochromeDisplay.cpp -o CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.s

# Object files for target IBMPcEmu
IBMPcEmu_OBJECTS = \
"CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.o" \
"CMakeFiles/IBMPcEmu.dir/src/main.cpp.o" \
"CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.o"

# External object files for target IBMPcEmu
IBMPcEmu_EXTERNAL_OBJECTS =

IBMPcEmu: CMakeFiles/IBMPcEmu.dir/src/cpu8086.cpp.o
IBMPcEmu: CMakeFiles/IBMPcEmu.dir/src/main.cpp.o
IBMPcEmu: CMakeFiles/IBMPcEmu.dir/src/monochromeDisplay.cpp.o
IBMPcEmu: CMakeFiles/IBMPcEmu.dir/build.make
IBMPcEmu: CMakeFiles/IBMPcEmu.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/iliashdz/IBMPcEmu/bin/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Linking CXX executable IBMPcEmu"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/IBMPcEmu.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/IBMPcEmu.dir/build: IBMPcEmu
.PHONY : CMakeFiles/IBMPcEmu.dir/build

CMakeFiles/IBMPcEmu.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/IBMPcEmu.dir/cmake_clean.cmake
.PHONY : CMakeFiles/IBMPcEmu.dir/clean

CMakeFiles/IBMPcEmu.dir/depend:
	cd /home/iliashdz/IBMPcEmu/bin && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/iliashdz/IBMPcEmu /home/iliashdz/IBMPcEmu /home/iliashdz/IBMPcEmu/bin /home/iliashdz/IBMPcEmu/bin /home/iliashdz/IBMPcEmu/bin/CMakeFiles/IBMPcEmu.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/IBMPcEmu.dir/depend

