package com.MediaStack.MediaStack.ui;

import com.MediaStack.MediaStack.entity.model.director.Director;
import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileModel;
import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileTypeEnum;
import com.MediaStack.MediaStack.service.MediaFileService;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.H1;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.component.upload.receivers.MemoryBuffer;
import com.vaadin.flow.dom.ThemeList;
import com.vaadin.flow.router.Route;

import com.vaadin.flow.theme.lumo.Lumo;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.Collection;

@Route("")
public class MainView extends VerticalLayout {

    private final MediaFileService mediaService;
    private final Grid<MediaFileModel> grid = new Grid<>(MediaFileModel.class);

    @Autowired
    public MainView(MediaFileService mediaService) {
        this.mediaService = mediaService;

        add(new H1("MediaStack"));

        setupColoringButton();
        setupUpload();
        setupGrid();

        refreshGrid();
    }

    public void setupColoringButton() {
        Button toggleButton = new Button("Toggle theme variant", click -> {
            ThemeList themeList = UI.getCurrent().getElement().getThemeList();

            if (themeList.contains(Lumo.DARK)) {
                themeList.remove(Lumo.DARK);
            } else {
                themeList.add(Lumo.DARK);
            }
        });

        add(toggleButton);
    }

    private void setupUpload() {
        MemoryBuffer buffer = new MemoryBuffer();
        Director mediaDirector = new Director();
        Upload upload = new Upload(buffer);
        upload.setAcceptedFileTypes("image/*", "video/*", "application/pdf");
        upload.addSucceededListener(event -> {
            try {
                Path uploadDir = Paths.get("uploads");
                if (!Files.exists(uploadDir)) {
                    Files.createDirectories(uploadDir);
                }
                Path filePath = uploadDir.resolve(event.getFileName());
                Files.copy(buffer.getInputStream(), filePath, java.nio.file.StandardCopyOption.REPLACE_EXISTING);

                MediaFileModel mediaFile;
                String mimeType = event.getMIMEType();
                if (mimeType.startsWith("image/")) {
                    mediaFile = mediaDirector.constructImageFileModel(event.getFileName(), filePath.toString());
                } else if (mimeType.startsWith("video/")) {
                    mediaFile = mediaDirector.constructVideoFileModel(event.getFileName(), filePath.toString());
                } else if (mimeType.equals("application/pdf")) {
                    mediaFile = mediaDirector.constructPdfFileModel(event.getFileName(), filePath.toString());
                } else {
                    throw new IllegalArgumentException("Unsupported file type: " + mimeType);
                }
                mediaService.createMediaFile(mediaFile);
                Notification.show("File uploaded!");
                refreshGrid();
            } catch (IOException | IllegalArgumentException e) {
                Notification.show("Upload failed: ");
            }
        });
        add(upload);
    }

    private String getFileType(String mimeType) {
        if (mimeType.startsWith("image/")) return "IMAGE";
        if (mimeType.startsWith("video/")) return "VIDEO";
        if (mimeType.equals("application/pdf")) return "PDF";
        throw new IllegalArgumentException("Unsupported file type: " + mimeType);
    }

    private void setupGrid() {
        grid.setColumns("id", "name", "fileType", "uploadDate");
        grid.addComponentColumn(mediaFile -> {
            Button delete = new Button("Delete", e -> {
                mediaService.deleteMediaFileById(mediaFile.getId());
                refreshGrid();
            });
            return delete;
        }).setHeader("Actions");
        add(grid);
    }

    private void refreshGrid() {
        Collection<MediaFileModel> files = mediaService.getAllMediaFiles();
        grid.setItems(files);
    }
}