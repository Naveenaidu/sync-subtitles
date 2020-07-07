
# sync-subtitles

## Introduction

No more worries about the unsynched subtitles. sync-subtitles is a new reformed software that will do **state of the art video and audio processing** to automatically sync the subtitles.

To run the file:
```
Wait until someone comes up with the actual technique xD
```

Heheh! Just Kidding xD. Jokes Apart! This is the plain old subtitle syncher where you still need to find out the offset of the subtitle using the synchronize tool provided in many video player.

 Once you find the correct offset, you can use this tool to automatically adjust the timestamp in subtitles.

## Usage

```bash
cat "subtitleFile.srt" | ./sync-subtitle offset > "newfile.srt"
```

#### To delay the subtitle
```bash
cat "dumbo.srt" | ./sync-subtitle 100 > "newfile.srt"
```

#### To hasten the subtitle
```bash
cat "dumbo.srt" | ./sync-subtitle -100 > "newfile.srt"
```

