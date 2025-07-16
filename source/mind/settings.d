module mind.settings;

public final class Settings {
    string[] sourceFiles;
    bool isVerbose;
    bool is64Bit;
}

Settings _settings;

void setSettings(Settings settings) {
    _settings = settings;
}

Settings getSettings() {
    return _settings;
}