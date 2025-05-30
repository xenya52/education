package com.MediaStack.MediaStack.MediaFileModel;

import org.junit.jupiter.api.Test;

import org.springframework.boot.test.context.SpringBootTest;

import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileModel;
import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileTypeEnum;
import com.MediaStack.MediaStack.entity.model.director.Director;

@SpringBootTest
public class MediaFileModelDirectorTest {

    @Test
    void testConstructVideoFileModel() {
        Director mediaFileDirector = new Director();
        MediaFileModel mediaFile = mediaFileDirector.constructVideoFileModel("12345", "Sample Media Video", "2025-05-30", "exampleName/mp4");

        assert mediaFile.getFileType().equals(MediaFileTypeEnum.VIDEO_MP4);
    }

    @Test
    void testConstructImageFileModel() {
        Director mediaFileDirector = new Director();
        MediaFileModel mediaFile = mediaFileDirector.constructImageFileModel("12345", "Sample Media Image", "2025-05-30", "exampleName/jpg");

        assert mediaFile.getFileType().equals(MediaFileTypeEnum.IMAGE_JPG);
    }

    @Test
    void testConstructPdfFileModel() {
        Director mediaFileDirector = new Director();
        MediaFileModel mediaFile = mediaFileDirector.constructPdfFileModel("12345", "Sample Media PDF", "2025-05-30", "exampleName/pdf");

        assert mediaFile.getFileType().equals(MediaFileTypeEnum.PDF);
    }
}